return {
  "stevearc/oil.nvim",
  config = function()
    vim.keymap.set("n", "<Leader>e", ":Oil<CR>", { desc = "File [E]xplorer" })
    require("oil").setup({
      view_options = {
        show_hidden = true,
      },
    })
  end,
  -- TODO: DECIDE if I want to remove
  -- nvim-tree as backup
  {
    "kyazdani42/nvim-tree.lua",
    config = function()
      vim.keymap.set("n", "<Leader>E", ":NvimTreeToggle<CR>", { desc = "File [E]xplorer Tree" })
      require("nvim-tree").setup({})
    end,
  },
}
