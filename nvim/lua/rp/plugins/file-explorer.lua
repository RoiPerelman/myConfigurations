return {
  "stevearc/oil.nvim",
  config = function()
    require("oil").setup({
      view_options = {
        show_hidden = true,
      },
      keymaps = {
        ["g?"] = "actions.show_help",
        ["<CR>"] = "actions.select",
        ["<C-v>"] = "actions.select_vsplit",
        ["<C-s>"] = "actions.select_split",
        ["<C-t>"] = "actions.select_tab",
        ["<C-p>"] = "actions.preview",
        ["<C-c>"] = "actions.close",
        ["<C-r>"] = "actions.refresh",
        ["-"] = "actions.parent",
        ["_"] = "actions.open_cwd",
        ["`"] = "actions.cd",
        ["~"] = "actions.tcd",
        ["gs"] = "actions.change_sort",
        ["gx"] = "actions.open_external",
        ["g."] = "actions.toggle_hidden",
        ["g\\"] = "actions.toggle_trash",
      },
      use_default_keymaps = false,
    })
    require("oil.util").run_after_load(0, function()
      require("oil").select({ preview = true })
    end)
    vim.keymap.set("n", "<Leader>e", ":Oil<CR>", { desc = "File [E]xplorer" })
  end,
  -- TODO: DECIDE if I want to remove
  -- nvim-tree as backup
  -- {
  --   "kyazdani42/nvim-tree.lua",
  --   config = function()
  --     require("nvim-tree").setup({})
  --     vim.keymap.set("n", "<Leader>E", ":NvimTreeToggle<CR>", { desc = "File [E]xplorer Tree" })
  --   end,
  -- },
}
