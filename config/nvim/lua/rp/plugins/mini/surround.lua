local surround = require('mini.surround')
surround.setup({
  mappings = {
    add = "ys",     -- Add surrounding in Normal and Visual modes
    delete = "ds",  -- Delete surrounding
    replace = "cs", -- Replace surrounding
  },
})
