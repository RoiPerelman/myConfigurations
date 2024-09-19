-- Define your color palette
local colors = {
  bg = "#2a2c2d",      -- _very-dark-gray (background)
  fg = "#E6E6E6",      -- _very-light-gray (foreground)
  red = "#FF2C6D",     -- _red
  green = "#19f9d8",   -- _green
  yellow = "#FFB86C",  -- _orange
  blue = "#45A9F9",    -- _blue
  magenta = "#B084EB", -- _purple
  cyan = "#6FE7D2",    -- _light-green
  white = "#E6E6E6",   -- _very-light-gray (same as foreground)
}
-- Set highlight groups
local function set_highlights()
  local hl = vim.api.nvim_set_hl

  hl(0, "Normal", { fg = colors.fg, bg = colors.bg })
  hl(0, "Comment", { fg = colors.green, italic = true })
  hl(0, "Constant", { fg = colors.magenta })
  hl(0, "String", { fg = colors.green })
  hl(0, "Function", { fg = colors.blue })
  hl(0, "Keyword", { fg = colors.red, bold = true })
  -- Add more highlight groups as needed
end

-- Apply the color scheme
local function load_colorscheme()
  vim.cmd("hi clear")
  if vim.fn.exists("syntax_on") then
    vim.cmd("syntax reset")
  end
  vim.o.background = "dark"
  vim.g.colors_name = "rp" -- Set the colorscheme name

  set_highlights()
end

-- Load the colorscheme
load_colorscheme()
