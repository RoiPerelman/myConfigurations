return { -- Fuzzy Finder (files, lsp, etc)
	"nvim-telescope/telescope.nvim",
	event = "VimEnter",
	branch = "0.1.x",
	dependencies = {
		"nvim-lua/plenary.nvim",
		{
			"nvim-telescope/telescope-fzf-native.nvim",
			build = "make",
			-- `cond` is used to determine whether this plugin should be installed and loaded.
			cond = function()
				return vim.fn.executable("make") == 1
			end,
		},
		{ "nvim-telescope/telescope-ui-select.nvim" },
		{ "nvim-tree/nvim-web-devicons", enabled = vim.g.have_nerd_font },
	},
	config = function()
		-- opens a window that shows you all of the keymaps for the current Telescope picker
		--  - Insert mode: <c-/>
		--  - Normal mode: ?
		--
		-- See `:help telescope` and `:help telescope.setup()`
		require("telescope").setup({
			defaults = {
				sorting_strategy = "ascending",
				layout_config = {
					prompt_position = "top",
				},
			},
			extensions = {
				["ui-select"] = {
					require("telescope.themes").get_dropdown(),
				},
			},
		})

		-- Enable Telescope extensions if they are installed
		pcall(require("telescope").load_extension, "fzf")
		pcall(require("telescope").load_extension, "ui-select")

		-- See `:help telescope.builtin`
		local builtin = require("telescope.builtin")
		vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "[F]ind [H]elp" })
		vim.keymap.set("n", "<leader>fk", builtin.keymaps, { desc = "[F]ind [K]eymaps" })
		vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "[F]ind [F]iles" })
		vim.keymap.set("n", "<leader>ft", builtin.builtin, { desc = "[F]ind [T]elescope builtin functions" })
		vim.keymap.set("n", "<leader>fw", builtin.grep_string, { desc = "[F]ind current [W]ord" })
		vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "[F]ind using [G]rep" })
		vim.keymap.set("n", "<leader>fd", builtin.diagnostics, { desc = "[F]ind [D]iagnostics" })
		vim.keymap.set("n", "<leader>fe", builtin.oldfiles, { desc = "[F]ind Recent [E]dited Files" })
		vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "[F]ind [B]uffers" })
		vim.keymap.set("n", "<leader>fq", builtin.quickfix, { desc = "[F]ind [Q]uickfix" })
		vim.keymap.set("n", "<Leader>fm", builtin.marks, { desc = "[F]ind [M]arks" })
		vim.keymap.set("n", "<Leader>fj", ":Telescope jumplist<CR>", { desc = "[F]ind [J]umplist" })
		vim.keymap.set(
			"n",
			"<Leader>fr",
			':lua require"telescope.builtin".grep_string({ use_regex = true, search = vim.fn.input("Grep for > ")})<CR>',
			{ desc = "[F]ind [R]egex and after live grep" }
		)

		-- Slightly advanced example of overriding default behavior and theme
		vim.keymap.set("n", "<leader>/", function()
			-- You can pass additional configuration to Telescope to change the theme, layout, etc.
			builtin.current_buffer_fuzzy_find(require("telescope.themes").get_dropdown({
				winblend = 10,
				previewer = false,
			}))
		end, { desc = "[/] Fuzzily search in current buffer" })

		-- It's also possible to pass additional configuration options.
		--  See `:help telescope.builtin.live_grep()` for information about particular keys
		vim.keymap.set("n", "<leader>f/", function()
			builtin.live_grep({
				grep_open_files = true,
				prompt_title = "Live Grep in Open Files",
			})
		end, { desc = "[F]ind [/] in Open Files" })

		-- Shortcut for searching your Neovim configuration files
		vim.keymap.set("n", "<leader>fp", function()
			builtin.find_files({ cwd = vim.fn.stdpath("config") })
		end, { desc = "[F]ind files in [P]rivate config" })

		-- TODO: move to a different file
		--
		local inspekto_filepaths = {
			"~/tiny_inspektor/sw/fixi",
			"~/tiny_inspektor/sw/fixi_client",
			"~/tiny_inspektor/sw/tiny_database",
			"~/tiny_inspektor/sw/tier2/tiny_std",
			"~/tiny_inspektor/sw/tier2/common_std",
			"~/tiny_inspektor/sw/data_coordinator",
			"~/tiny_inspektor/sw/connectivity",
			"~/tiny_inspektor/sw/inspekto_agent",
			"~/tiny_inspektor/sw/integration_managers",
		}
		vim.keymap.set("n", "<leader>fif", function()
			builtin.find_files({
				search_dirs = inspekto_filepaths,
				additional_args = { "--hidden" },
				-- find_command = {
				-- 	"rg",
				-- 	"--files",
				-- 	"--iglob",
				-- 	"!.git", -- Explicitly ignore .git directory (usually not needed as rg does this by default)
				-- 	"--hidden", -- Optional: Remove this line if you do not want to include hidden files
				-- },
			})
		end, { desc = "[F]ind [I]nspekto [F]iles" })
		vim.keymap.set("n", "<leader>fig", function()
			builtin.live_grep({
				search_dirs = inspekto_filepaths,
				-- path_display = { "shorten" },
				path_display = { "tail" },
				-- path_display = function(opts, path)
				-- 	-- Define the subpath to start displaying from
				-- 	local subpath_start = "sw/"
				-- 	-- Find the position where this subpath starts
				-- 	local start_pos = path:find(subpath_start)
				-- 	if start_pos then
				-- 		-- Adjust the path to start from the specified subpath
				-- 		return path:sub(start_pos + #subpath_start - 1)
				-- 	end
				-- 	-- If the specific subpath isn't found, return the whole path
				-- 	return path
				-- end,
				additional_args = { "--hidden", "--ignore-case" }, -- Specified as a table directly
			})
		end, { desc = "[F]ind [I]nspekto [G]rep" })
	end,
}
