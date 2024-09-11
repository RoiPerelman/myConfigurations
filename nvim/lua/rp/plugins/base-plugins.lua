return {
  -- lua functions that many plugins use
  { "nvim-lua/plenary.nvim" },
  -- ui components that many plugins use
  { "MunifTanjim/nui.nvim", lazy = true },
  -- icons that many plugins use
  { "kyazdani42/nvim-web-devicons" },
  -- { "echasnovski/mini.icons", version = false },

  -- -- configure luals for editing Neovim config
  -- {
  --   "folke/lazydev.nvim",
  --   ft = "lua", -- only load on lua files
  --   opts = {
  --     enable = true,
  --     library = {
  --       { path = "luvit-meta/library", words = { "vim%.uv" } },
  --     },
  --   },
  -- },
  -- { "Bilal2453/luvit-meta", lazy = true }, -- optional `vim.uv` typings
  -- { -- optional completion source for require statements and module annotations
  --   "hrsh7th/nvim-cmp",
  --   opts = function(_, opts)
  --     opts.sources = opts.sources or {}
  --     table.insert(opts.sources, {
  --       name = "lazydev",
  --       group_index = 0, -- set group index to 0 to skip loading LuaLS completions
  --     })
  --   end,
  -- },
}
