return { -- Fuzzy Finder (files, lsp, etc)
	"nvim-telescope/telescope.nvim",
	event = "VimEnter",
	branch = "0.1.x",
	dependencies = {
		"nvim-lua/plenary.nvim",
		{ -- If encountering errors, see telescope-fzf-native README for installation instructions
			"nvim-telescope/telescope-fzf-native.nvim",

			-- `build` is used to run some command when the plugin is installed/updated.
			-- This is only run then, not every time Neovim starts up.
			build = "make",

			-- `cond` is a condition used to determine whether this plugin should be
			-- installed and loaded.
			cond = function()
				return vim.fn.executable("make") == 1
			end,
		},
		{ "nvim-telescope/telescope-ui-select.nvim" },

		-- Useful for getting pretty icons, but requires a Nerd Font.
		{ "nvim-tree/nvim-web-devicons", enabled = vim.g.have_nerd_font },
	},
	config = function()
		-- opens a window that shows you all of the keymaps for the current Telescope picker
		--  - Insert mode: <c-/>
		--  - Normal mode: ?
		--
		-- See `:help telescope` and `:help telescope.setup()`
		local actions = require("telescope.actions")
		require("telescope").setup({
			-- defaults = {
			-- 	vimgrep_arguments = {
			-- 		'rg',
			-- 		'--color=never',
			-- 		'--no-heading',
			-- 		'--with-filename',
			-- 		'--line-number',
			-- 		'--column',
			-- 		'--smart-case'
			-- 	},
			-- 	prompt_position = "top", -- make prompt be at the top
			-- 	prompt_prefix = "> ",
			-- 	selection_caret = "> ",
			-- 	entry_prefix = "  ",
			-- 	initial_mode = "insert",
			-- 	selection_strategy = "reset",
			-- 	sorting_strategy = "ascending", -- results widow sorting from top to bottom
			-- 	layout_strategy = "horizontal",
			-- 	layout_defaults = {
			-- 		horizontal = {
			-- 			preview_width = 0.55, -- percentage of width preview takes
			-- 			-- width_padding = 0.1,
			-- 			-- height_padding = 0.05,
			-- 		},
			-- 	},
			-- 	file_sorter =  require'telescope.sorters'.get_fuzzy_file,
			-- 	file_ignore_patterns = {},
			-- 	generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
			-- 	shorten_path = true,
			-- 	winblend = 0,
			-- 	width = 0.75,
			-- 	preview_cutoff = 120,
			-- 	results_height = 1,
			-- 	results_width = 0.8,
			-- 	border = {},
			-- 	borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
			-- 	color_devicons = true,
			-- 	use_less = true,
			-- 	set_env = { ['COLORTERM'] = 'truecolor' }, -- default = nil,
			-- 	file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
			-- 	grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
			-- 	qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,
			--
			-- 	-- Developer configurations: Not meant for general override
			-- 	buffer_previewer_maker = require'telescope.previewers'.buffer_previewer_maker,
			-- 	mappings = {
			-- 		i = {
			-- 			["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
			-- 		},
			-- 		n = {
			-- 			["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
			-- 		}
			-- 	}
			-- },
			-- defaults = {
			-- 	vimgrep_arguments = {
			-- 		"rg",
			-- 		"--follow",
			-- 		"--color=never",
			-- 		"--no-heading",
			-- 		"--with-filename",
			-- 		"--line-number",
			-- 		"--column",
			-- 		"--smart-case",
			-- 		"--trim", -- add this value
			-- 	},
			-- 	sorting_strategy = "ascending",
			-- 	layout_config = {
			-- 		prompt_position = "top",
			-- 	},
			-- },
			-- pickers = {
			-- 	find_files = {
			-- 		-- find_command = { "fd", "--type", "f", "--strip-cwd-prefix", "--follow", "--exclude .git" },
			-- 		-- find_command = { "fd", "--type", "f", "--strip-cwd-prefix", "--follow" },
			-- 		find_command = { "fd", "--type", "f", "--follow" },
			-- 	},
			-- },
			-- pickers = {}
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
		vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "[F]ind by [G]rep" })
		vim.keymap.set("n", "<leader>fd", builtin.diagnostics, { desc = "[F]ind [D]iagnostics" })
		vim.keymap.set("n", "<leader>fe", builtin.oldfiles, { desc = "[F]ind Recent Files" })
		vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "[F]ind buffers" })
		vim.keymap.set("n", "<leader>fq", builtin.quickfix, { desc = "[F]ind quickfix" })
		vim.keymap.set("n", "<Leader>fm", ":Telescope marks<CR>", { desc = "[F]ind marks" })
		vim.keymap.set("n", "<Leader>fj", ":Telescope jumplist<CR>", { desc = "[F]ind jumplist" })
		vim.keymap.set(
			"n",
			"<Leader>fr",
			':lua require"telescope.builtin".grep_string({ use_regex = true, search = vim.fn.input("Grep for > ")})<CR>',
			{ desc = "[F]ind regex" }
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
		vim.keymap.set("n", "<leader>fn", function()
			builtin.find_files({ cwd = vim.fn.stdpath("config") })
		end, { desc = "[F]ind [N]eovim files" })
	end,
}
