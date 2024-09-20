return {
  "stevearc/oil.nvim",
  config = function()
    require("oil").setup({
      view_options = {
        show_hidden = true,
      },
      keymaps = {
        ["<C-v>"] = "actions.select_vsplit",
      },
      -- use_default_keymaps = false,
    })
    -- require("oil.util").run_after_load(0, function()
    --   require("oil").open_preview()
    -- end)
    vim.keymap.set("n", "<Leader>e", ":Oil<CR>", { desc = "[E]xplorer (file)" })
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
