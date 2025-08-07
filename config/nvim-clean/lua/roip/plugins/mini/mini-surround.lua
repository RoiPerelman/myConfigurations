---@brief
--- adds surround funcionality

vim.pack.add({{ src = "https://github.com/echasnovski/mini.surround", version = "stable" }})

local surround = require('mini.surround')
surround.setup({
  mappings = {
    add = "ys",     -- Add surrounding in Normal and Visual modes
    delete = "ds",  -- Delete surrounding
    replace = "cs", -- Replace surrounding
  },
})
