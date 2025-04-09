require('blink.cmp').setup({
  sources = {
    default = { "lsp", "path", "snippets", "buffer", "lazydev" },
    providers = {
      lazydev = {
        name = "LazyDev",
        module = "lazydev.integrations.blink",
        -- make lazydev completions top priority (see `:h blink.cmp`)
        score_offset = 100,
      },
    }
  },
  completion = {
    documentation = {
      auto_show = true
    }
  },
  signature = {
    enabled = true
  }
})
