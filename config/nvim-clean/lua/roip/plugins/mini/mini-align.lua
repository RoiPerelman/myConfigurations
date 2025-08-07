---@brief
--- help align according to different split options and justification
--- default mappings ga or gA with preview

vim.pack.add({{ src = "https://github.com/echasnovski/mini.align", version = "stable" }})

local align = require('mini.align')
align.setup({})
