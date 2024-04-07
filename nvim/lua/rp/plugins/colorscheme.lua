return {
  {
    "dracula/vim",
    -- priority = 1000, -- Make sure to load this before all the other start plugins.
    -- init = function()
    --   vim.cmd.colorscheme("dracula")
    --   vim.cmd.hi("Comment gui=none")
    -- end,
  },
  {
    "folke/tokyonight.nvim",
    priority = 1000, -- Make sure to load this before all the other start plugins.
    init = function()
      vim.cmd.colorscheme("tokyonight-night")
      vim.cmd.hi("Comment gui=none")
    end,
  },
  {
    "sainnhe/sonokai",
    -- priority = 1000, -- Make sure to load this before all the other start plugins.
    -- init = function()
    --   vim.cmd.colorscheme("sonokai")
    --   vim.cmd.hi("Comment gui=none")
    -- end,
  },
  {
    "arcticicestudio/nord-vim",
    -- priority = 1000, -- Make sure to load this before all the other start plugins.
    -- init = function()
    --   vim.cmd.colorscheme("nord")
    --   vim.cmd.hi("Comment gui=none")
    -- end,
  },
}
