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
    {
      "folke/trouble.nvim",
      dependencies = { "nvim-tree/nvim-web-devicons" },
      config = function()
        require("trouble").setup({})

        vim.keymap.set("n", "<leader>xx", function()
          require("trouble").toggle()
        end)
        vim.keymap.set("n", "<leader>xw", function()
          require("trouble").toggle("workspace_diagnostics")
        end)
        vim.keymap.set("n", "<leader>xd", function()
          require("trouble").toggle("document_diagnostics")
        end)
        vim.keymap.set("n", "<leader>xq", function()
          require("trouble").toggle("quickfix")
        end)
        vim.keymap.set("n", "<leader>xl", function()
          require("trouble").toggle("loclist")
        end)
        vim.keymap.set("n", "gR", function()
          require("trouble").toggle("lsp_references")
        end)
      end,
    },
  },
  config = function()
    local trouble = require("trouble.providers.telescope")

    require("telescope").setup({
      defaults = {
        sorting_strategy = "ascending",
        layout_config = {
          prompt_position = "top",
        },
        mappings = {
          i = { ["<c-t>"] = trouble.open_with_trouble },
          n = { ["<c-t>"] = trouble.open_with_trouble },
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
      "~/tiny_inspektor/cicd/tinybox",
      "~/tiny_inspektor/sw/fixi",
      "~/tiny_inspektor/sw/fixi_client",
      "~/tiny_inspektor/sw/tiny_database",
      "~/tiny_inspektor/sw/tier2/tiny_std",
      "~/tiny_inspektor/sw/tier2/common_std",
      "~/tiny_inspektor/sw/data_coordinator",
      "~/tiny_inspektor/sw/connectivity",
      "~/tiny_inspektor/sw/inspekto_agent",
      "~/tiny_inspektor/sw/integration_managers",
      "~/tiny_inspektor/sw/profile_center",
      "~/tiny_inspektor/sw/inspekto_agent",
    }
    local inspekto_path_display = function(_, path)
      -- Define the subpath to start displaying from
      local sw_subpath_start = "sw/"
      local cicd_subpath_start = "cicd/"
      -- Find the position where this subpath starts
      local sw_start_pos = path:find(sw_subpath_start)
      local cicd_start_pos = path:find(cicd_subpath_start)
      if sw_start_pos then
        -- Adjust the path to start from the end of the specified subpath
        return path:sub(sw_start_pos + #sw_subpath_start)
      elseif cicd_start_pos then
        -- Same for cicd subpath
        return path:sub(cicd_start_pos + #cicd_subpath_start)
      end
      -- If the specific subpath isn't found, return the whole path
      return path
    end
    vim.keymap.set("n", "<leader>if", function()
      builtin.find_files({
        search_dirs = inspekto_filepaths,
        additional_args = { "--hidden" },
        path_display = inspekto_path_display,
      })
    end, { desc = "[I]nspekto [F]iles" })
    vim.keymap.set("n", "<leader>ig", function()
      builtin.live_grep({
        search_dirs = inspekto_filepaths,
        path_display = inspekto_path_display,
        -- additional_args = { "--hidden", "--ignore-case" }, -- Specified as a table directly
      })
    end, { desc = "[I]nspekto [G]rep" })
    vim.keymap.set("n", "<leader>iw", function()
      builtin.grep_string({
        search_dirs = inspekto_filepaths,
        path_display = inspekto_path_display,
        -- additional_args = { "--hidden", "--ignore-case" }, -- Specified as a table directly
      })
    end, { desc = "[I]nspekto [W]ord" })
  end,
}
