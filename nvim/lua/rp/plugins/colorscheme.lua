return { -- You can easily change to a different colorscheme.
  -- "folke/tokyonight.nvim",
  -- "dracula/vim",
  "Mofiqul/dracula.nvim",
  -- "sainnhe/sonokai",
  priority = 1000, -- Make sure to load this before all the other start plugins.
  init = function()
    -- vim.cmd.colorscheme("tokyonight")
    vim.cmd.colorscheme("dracula")
    -- vim.cmd.colorscheme("sonokai")
    vim.cmd.hi("Comment gui=none")
  end,
}
