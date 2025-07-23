require("tokyonight").setup({
  style = "night", -- moon is also good
  on_colors = function(colors)
    -- colors.bg = "#1a1b26" -- set background color
    -- colors.fg = "#c0caf5" -- set foreground color
    -- colors.bg = "#f7768e" -- set red color
    -- colors.green = "#9ece6a" -- set green color
    -- colors.blue = "#7aa2f7" -- set blue color
    -- colors.yellow = "#e0af68" -- set yellow color
  end,
  on_highlights = function(hl, c)
    -- hl.NonText = { fg = c.red } -- set non-text color
    -- hl.SpecialKey = { fg = c.yellow, bold = true }
  end,
})
vim.cmd.colorscheme("tokyonight")
