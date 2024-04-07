return {
  "stevearc/oil.nvim",
  config = function()
    require("oil").setup({
      view_options = {
        show_hidden = true,
      },
    })
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
