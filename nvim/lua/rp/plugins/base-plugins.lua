return {
  -- lua functions that many plugins use
  "nvim-lua/plenary.nvim",
  -- icons that many plugins use
  "kyazdani42/nvim-web-devicons",
  {
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    opts = {
      enable = true,
      library = {
        { path = "luvit-meta/library", words = { "vim%.uv" } },
      },
    },
  },
  { "Bilal2453/luvit-meta", lazy = true }, -- optional `vim.uv` typings
}
