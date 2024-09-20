return {
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000, -- Make sure to load this before all the other start plugins.
    config = function()
      require("tokyonight").setup({
        style = "night", -- moon is also good
      })
      vim.cmd.colorscheme("tokyonight")
    end,
  },
  { "dracula/vim" },
  { "sainnhe/sonokai" },
}
