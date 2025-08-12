-- Define your color palette
local colors = {
  -- Set highlight group  -- Gray Scale
  very_light_gray = '#E6E6E6', -- Primary font color
  very_light_gray_fade = '#FFFFFF',
  lighter_gray = '#CDCDCD',
  lighter_gray_fade = '#CDCDCD30',
  contrast_gray = '#BBBBBB',
  light_gray = '#757575',
  light_midnight = '#676B79',
  steel_gray = '#3E4250',
  gray = '#373B41',
  gray_fade = '#373B4199',
  seal = '#31353a',           -- Light background
  very_dark_gray = '#2a2c2d', -- Dark background
  charcoal = '#222223',       -- Border between gray backgrounds

  -- Blue+Purple+Green
  blue = '#45A9F9',
  light_blue = '#6FC1FF',
  purple = '#B084EB',
  purple_fade = '#B084EB60',
  light_purple = '#BCAAFE',
  green = '#19f9d8',
  light_green = '#6FE7D2',
  green_fade = '#19f9d899',

  -- Pink+Red+Orange
  red = '#FF2C6D',
  light_red = '#FF4B82',
  orange = '#FFB86C',
  light_orange = '#FFCC95',
  light_orange_fade = '#FFCC9560',
  pink = '#FF75B5',
  light_pink = '#FF9AC1',
  light_pink_fade = '#FF9AC170',

  -- Element Colors
  background_dark = '#242526',  -- very-dark-gray
  background_light = '#292A2B', -- seal
  foreground = '#E6E6E6',       -- very-light-gray
  diff_green = '#19f9d866',
  diff_red = '#FF4B8266',
  merge_current_header = '#B084EB90',   -- purple with alpha
  merge_current_content = '#B084EB40',  -- same purple, less alpha
  merge_incoming_header = '#45A9F990',  -- blue with alpha
  merge_incoming_content = '#FFB86C40', -- same orange, less alpha
  transparent = '#00000000',

  -- Git Colors
  git_modified = '#FFCC95', -- light-orange
  git_added = '#19f9d8',    -- green
  git_removed = '#FF4B82',  -- light-red
  git_ignored = '#757575',  -- light-gray

  -- Linter Colors
  error = '#FF4B82',   -- light-red
  warning = '#FFCC95', -- light-orange
  info = '#6FC1FF',    -- light-blue
}


local function set_highlights()
  local hl = vim.api.nvim_set_hl

  hl(0, "Normal", { fg = nil, bg = colors.light })
  hl(0, "ColorColumn", { fg = nil, bg = colors.grey })
  -- hl(0, "Conceal", { fg = colors['blue'], bg = nil })
  hl(0, "CurSearch", { fg = colors['seal'], bg = colors['error'] })
  hl(0, "Cursor", { fg = colors['git_removed'], bg = colors['git_added'] })
  -- hl(0, "CursorColumn", { fg = nil, bg = colors['seal'] })
  -- hl(0, "CursorIM", { fg = colors['very_light_gray'], bg = colors['gray'] })
  -- hl(0, "CursorLine", { fg = nil, bg = colors['seal'] })
  -- hl(0, "CursorLineFold", { fg = colors['light_purple'], bg = colors['seal'] })
  -- hl(0, "CursorLineNr", { fg = colors['lighter_gray'], bg = colors['seal'] })
  -- hl(0, "CursorLineSign", { fg = colors['light_gray'], bg = colors['seal'] })
  -- hl(0, "DiffAdd", { fg = colors['git_added'], bg = colors['seal'] })
  -- hl(0, "DiffChange", { fg = colors['purple'], bg = colors['seal'] })
  -- hl(0, "DiffDelete", { fg = colors['red'], bg = colors['seal'] })
  -- hl(0, "DiffText", { fg = colors['light_blue'], bg = colors['seal'] })
  -- hl(0, "Directory", { fg = colors['blue'], bg = nil })
  -- hl(0, "EndOfBuffer", { fg = colors['light_gray'], bg = nil })
  -- hl(0, "ErrorMsg", { fg = colors['red'], bg = colors['very_dark_gray'] })
  -- hl(0, "FoldColumn", { fg = colors['light_pink'], bg = colors['seal'] })
  -- hl(0, "Folded", { fg = colors['light_gray'], bg = colors['seal'] })
  -- hl(0, "IncSearch", { fg = colors['seal'], bg = colors['orange'] })
  -- hl(0, "lCursor", { fg = colors['very_light_gray'], bg = colors['gray'] })
  -- hl(0, "LineNr", { fg = colors['light_gray'], bg = colors['seal'] })
  -- hl(0, "LineNrAbove", { fg = colors['light_gray'], bg = colors['seal'] })
  -- hl(0, "LineNrBelow", { fg = colors['light_gray'], bg = colors['seal'] })
  -- hl(0, "MatchParen", { fg = nil, bg = colors['lighter_gray'] })
  -- hl(0, "ModeMsg", { fg = colors['green'], bg = nil })
  -- hl(0, "MoreMsg", { fg = colors['green'], bg = nil })
  -- hl(0, "MsgArea", { fg = colors['gray'], bg = colors['very_dark_gray'] })
  -- hl(0, "MsgSeparator", { fg = colors['lighter_gray'], bg = colors['lighter_gray'] })
  -- hl(0, "NonText", { fg = colors['light_gray'], bg = nil })
  -- hl(0, "Normal", { fg = colors['very_dark_gray'], bg = colors['very_dark_gray'] })
  -- hl(0, "NormalFloat", { fg = colors['gray'], bg = colors['seal'] })
  -- hl(0, "NormalNC", { fg = colors['gray'], bg = colors['very_dark_gray'] })
  -- hl(0, "Pmenu", { fg = colors['gray'], bg = colors['seal'] })
  -- hl(0, "PmenuExtra", { fg = colors['gray'], bg = colors['seal'] })
  -- hl(0, "PmenuExtraSel", { fg = colors['gray'], bg = colors['seal'] })
  -- hl(0, "PmenuKind", { fg = colors['gray'], bg = colors['seal'] })
  -- hl(0, "PmenuKindSel", { fg = colors['gray'], bg = colors['seal'] })
  -- hl(0, "PmenuMatch", { fg = colors['gray'], bg = colors['seal'] })
  -- hl(0, "PmenuMatchSel", { fg = colors['gray'], bg = colors['seal'] })
  -- hl(0, "PmenuSbar", { fg = nil, bg = colors['lighter_gray'] })
  -- hl(0, "PmenuSel", { fg = colors['gray'], bg = colors['seal'] })
  -- hl(0, "PmenuThumb", { fg = nil, bg = colors['lighter_gray'] })
  -- hl(0, "Question", { fg = colors['blue'], bg = nil })
  -- hl(0, "QuickFixLine", { fg = nil, bg = colors['seal'] })
  -- hl(0, "Search", { fg = colors['seal'], bg = colors['orange'] })
  -- hl(0, "SignColumn", { fg = colors['light_gray'], bg = colors['seal'] })
  -- hl(0, "SpecialKey", { fg = colors['light_gray'], bg = nil })
  -- hl(0, "SpellBad", { fg = nil, bg = nil, sp = colors['red'] })
  -- hl(0, "SpellCap", { fg = nil, bg = nil, sp = colors['blue'] })
  -- hl(0, "SpellLocal", { fg = nil, bg = nil, sp = colors['light_pink'] })
  -- hl(0, "SpellRare", { fg = nil, bg = nil, sp = colors['purple'] })
  -- hl(0, "StatusLine", { fg = colors['lighter_gray'], bg = colors['lighter_gray'] })
  -- hl(0, "StatusLineNC", { fg = colors['light_gray'], bg = colors['seal'] })
  -- hl(0, "Substitute", { fg = colors['seal'], bg = colors['orange'] })
  -- hl(0, "TabLine", { fg = colors['light_gray'], bg = colors['seal'] })
  -- hl(0, "TabLineFill", { fg = colors['light_gray'], bg = colors['seal'] })
  -- hl(0, "TabLineSel", { fg = colors['green'], bg = colors['seal'] })
  -- hl(0, "TermCursor", { fg = nil, bg = nil })
  -- hl(0, "TermCursorNC", { fg = nil, bg = nil })
  -- hl(0, "Title", { fg = colors['blue'], bg = nil })
  -- hl(0, "VertSplit", { fg = colors['lighter_gray'], bg = colors['lighter_gray'] })
  -- hl(0, "Visual", { fg = nil, bg = colors['lighter_gray'] })
  -- hl(0, "VisualNOS", { fg = colors['red'], bg = nil })
  -- hl(0, "WarningMsg", { fg = colors['red'], bg = nil })
  -- hl(0, "Whitespace", { fg = colors['light_gray'], bg = nil })
  -- hl(0, "WildMenu", { fg = colors['red'], bg = colors['orange'] })
  -- hl(0, "WinBar", { fg = colors['lighter_gray'], bg = colors['lighter_gray'] })
  -- hl(0, "WinBarNC", { fg = colors['light_gray'], bg = colors['seal'] })
  -- hl(0, "WinSeparator", { fg = colors['lighter_gray'], bg = colors['lighter_gray'] })
  --
  -- -- Standard syntax (affects treesitter)
  -- hl(0, "Boolean", { fg = colors['orange'], bg = nil })
  -- hl(0, "Character", { fg = colors['red'], bg = nil })
  -- hl(0, "Comment", { fg = colors['light_gray'], bg = nil })
  -- hl(0, "Conditional", { fg = colors['purple'], bg = nil })
  -- hl(0, "Constant", { fg = colors['orange'], bg = nil })
  -- hl(0, "Debug", { fg = colors['red'], bg = nil })
  -- hl(0, "Define", { fg = colors['purple'], bg = nil })
  -- hl(0, "Delimiter", { fg = colors['purple'], bg = nil })
  -- hl(0, "Error", { fg = colors['very_dark_gray'], bg = colors['red'] })
  -- hl(0, "Exception", { fg = colors['red'], bg = nil })
  -- hl(0, "Float", { fg = colors['orange'], bg = nil })
  -- hl(0, "Function", { fg = colors['blue'], bg = nil })
  -- hl(0, "Identifier", { fg = colors['red'], bg = nil })
  -- hl(0, "Ignore", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "Include", { fg = colors['blue'], bg = nil })
  -- hl(0, "Keyword", { fg = colors['blue'], bg = nil })
  -- hl(0, "Label", { fg = colors['orange'], bg = nil })
  -- hl(0, "Number", { fg = colors['orange'], bg = nil })
  -- hl(0, "Operator", { fg = colors['blue'], bg = nil })
  -- hl(0, "PreProc", { fg = colors['light_pink'], bg = nil })
  -- hl(0, "Repeat", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "Special", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "SpecialChar", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "Statement", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "StorageClass", { fg = colors['blue'], bg = nil })
  -- hl(0, "String", { fg = colors['green'], bg = nil })
  -- hl(0, "Structure", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "Tag", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "Todo", { fg = colors['orange'], bg = nil })
  -- hl(0, "Type", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "Typedef", { fg = colors['blue'], bg = nil })
  --
  -- -- Other from 'base16-vim'
  -- hl(0, "Bold", { fg = nil, bg = nil })
  -- hl(0, "Italic", { fg = nil, bg = nil })
  -- hl(0, "TooLong", { fg = colors['red'], bg = nil })
  -- hl(0, "Underlined", { fg = nil, bg = nil })
  --
  -- -- Patch diff
  -- hl(0, "diffAdded", { fg = colors['green'], bg = nil })
  -- hl(0, "diffChanged", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "diffFile", { fg = colors['blue'], bg = nil })
  -- hl(0, "diffLine", { fg = colors['light_blue'], bg = nil })
  -- hl(0, "diffRemoved", { fg = colors['red'], bg = nil })
  -- hl(0, "Added", { fg = colors['green'], bg = nil })
  -- hl(0, "Changed", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "Removed", { fg = colors['red'], bg = nil })
  --
  -- -- Git commit
  -- hl(0, "gitcommitBranch", { fg = colors['blue'], bg = nil })
  -- hl(0, "gitcommitComment", { link = 'Comment' })
  -- hl(0, "gitcommitDiscarded", { link = 'Comment' })
  -- hl(0, "gitcommitDiscardedFile", { fg = colors['red'], bg = nil })
  -- hl(0, "gitcommitDiscardedType", { fg = colors['blue'], bg = nil })
  -- hl(0, "gitcommitHeader", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "gitcommitOverflow", { fg = colors['red'], bg = nil })
  -- hl(0, "gitcommitSelected", { link = 'Comment' })
  -- hl(0, "gitcommitSelectedFile", { fg = colors['green'], bg = nil })
  -- hl(0, "gitcommitSelectedType", { link = 'gitcommitDiscardedType' })
  -- hl(0, "gitcommitSummary", { fg = colors['green'], bg = nil })
  -- hl(0, "gitcommitUnmergedFile", { link = 'gitcommitDiscardedFile' })
  -- hl(0, "gitcommitUnmergedType", { link = 'gitcommitDiscardedType' })
  -- hl(0, "gitcommitUntracked", { link = 'Comment' })
  -- hl(0, "gitcommitUntrackedFile", { fg = colors['yellow'], bg = nil })
  --
  -- -- Built-in diagnostic
  -- hl(0, "DiagnosticError", { fg = colors['red'], bg = nil })
  -- hl(0, "DiagnosticHint", { fg = colors['blue'], bg = nil })
  -- hl(0, "DiagnosticInfo", { fg = colors['light_blue'], bg = nil })
  -- hl(0, "DiagnosticOk", { fg = colors['green'], bg = nil })
  -- hl(0, "DiagnosticWarn", { fg = colors['light_purple'], bg = nil })
  --
  -- hl(0, "DiagnosticFloatingError", { fg = colors['red'], bg = colors['seal'] })
  -- hl(0, "DiagnosticFloatingHint", { fg = colors['blue'], bg = colors['seal'] })
  -- hl(0, "DiagnosticFloatingInfo", { fg = colors['light_blue'], bg = colors['seal'] })
  -- hl(0, "DiagnosticFloatingOk", { fg = colors['green'], bg = colors['seal'] })
  -- hl(0, "DiagnosticFloatingWarn", { fg = colors['light_purple'], bg = colors['seal'] })
  --
  -- hl(0, "DiagnosticSignError", { link = 'DiagnosticFloatingError' })
  -- hl(0, "DiagnosticSignHint", { link = 'DiagnosticFloatingHint' })
  -- hl(0, "DiagnosticSignInfo", { link = 'DiagnosticFloatingInfo' })
  -- hl(0, "DiagnosticSignOk", { link = 'DiagnosticFloatingOk' })
  -- hl(0, "DiagnosticSignWarn", { link = 'DiagnosticFloatingWarn' })
  --
  -- hl(0, "DiagnosticUnderlineError", { fg = nil, bg = nil, sp = colors['red'] })
  -- hl(0, "DiagnosticUnderlineHint", { fg = nil, bg = nil, sp = colors['blue'] })
  -- hl(0, "DiagnosticUnderlineInfo", { fg = nil, bg = nil, sp = colors['light_blue'] })
  -- hl(0, "DiagnosticUnderlineOk", { fg = nil, bg = nil, sp = colors['green'] })
  -- hl(0, "DiagnosticUnderlineWarn", { fg = nil, bg = nil, sp = colors['light_purple'] })
  --
  -- -- Built-in LSP
  -- hl(0, "LspReferenceText", { fg = nil, bg = colors['seal'] })
  -- hl(0, "LspReferenceRead", { link = 'LspReferenceText' })
  -- hl(0, "LspReferenceWrite", { link = 'LspReferenceText' })
  -- hl(0, "LspSignatureActiveParameter", { link = 'LspReferenceText' })
  -- hl(0, "LspCodeLens", { link = 'Comment' })
  -- hl(0, "LspCodeLensSeparator", { link = 'Comment' })
  --
  -- -- Tree-sitter
  -- hl(0, "@keyword.return", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "@symbol", { fg = colors['light_purple'], bg = nil })
  -- hl(0, "@variable", { fg = colors['gray'], bg = nil })
  --
  -- hl(0, "@text.strong", { fg = nil, bg = nil })
  -- hl(0, "@text.emphasis", { fg = nil, bg = nil })
  -- hl(0, "@text.strike", { fg = nil, bg = nil })
  -- hl(0, "@text.underline", { link = 'Underlined' })
  --
  -- -- Semantic tokens
  -- if vim.fn.has('nvim-0.9') == 1 then
  --   hl(0, "@lsp.type.variable", { fg = colors['gray'], bg = nil })
  --   hl(0, "@lsp.mod.defaultLibrary", { link = 'Special' })
  --   hl(0, "@lsp.mod.deprecated", { fg = colors['red'], bg = nil })
  -- end
  --
  -- -- New tree-sitter groups
  -- if vim.fn.has('nvim-0.10') == 1 then
  --   hl(0, "@markup.strong", { link = '@text.strong' })
  --   hl(0, "@markup.italic", { link = '@text.emphasis' })
  --   hl(0, "@markup.strikethrough", { link = '@text.strike' })
  --   hl(0, "@markup.underline", { link = '@text.underline' })
  --   hl(0, "@string.special.vimdoc", { link = 'SpecialChar' })
  --   hl(0, "@variable.parameter.vimdoc", { fg = colors['blue'], bg = nil })
  -- end
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
