return {
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    keys = {
      { "<Leader>gg", "<cmd>Neogit<cr>", desc = "Neo[G]it Magit" },
    },
    config = function()
      require("neogit").setup({})
    end,
  },
  {
    "FabijanZulj/blame.nvim",
    config = function()
      require("blame").setup()
      vim.keymap.set("n", "<Leader>gb", "<cmd>BlameToggle<cr>", { desc = "[G]it [B]lame Line Toggle" })
    end,
  },
}
