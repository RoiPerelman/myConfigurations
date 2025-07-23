local hipatterns = require('mini.hipatterns')
hipatterns.setup({
  highlighters = {
    -- Highlight 'FIXME', 'HACK', 'WARN', 'TODO', 'NOTE' with : at the end
    fixme     = { pattern = '()FIXME:()', group = 'DiagnosticError', extmark_opts = { sign_text = '', sign_hl_group = 'DiagnosticError' }, },
    hack      = { pattern = '()HACK:()', group = 'DiagnosticWarn', extmark_opts = { sign_text = '', sign_hl_group = 'DiagnosticWarn' } },
    warn      = { pattern = '()WARN:()', group = 'DiagnosticWarn', extmark_opts = { sign_text = '', sign_hl_group = 'DiagnosticWarn' } },
    todo      = { pattern = '()TODO:()', group = 'DiagnosticInfo', extmark_opts = { sign_text = '', sign_hl_group = 'DiagnosticInfo' } },
    note      = { pattern = '()NOTE:()', group = 'DiagnosticHint', extmark_opts = { sign_text = '', sign_hl_group = 'DiagnosticHint' } },

    -- Highlight hex color strings (RGB - `#ff0000`, `#00ff00`, `#0000ff`)
    hex_color = hipatterns.gen_highlighter.hex_color(),
  },
})

