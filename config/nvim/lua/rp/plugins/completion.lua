require('blink.cmp').setup({
  sources = {
    default = { "lsp", "path", "snippets", "buffer", "lazydev", "npm" },
    providers = {
      lazydev = {
        name = "LazyDev",
        module = "lazydev.integrations.blink",
        -- make lazydev completions top priority (see `:h blink.cmp`)
        score_offset = 100,
      },
      npm = {
        name = "npm",
        module = "blink-cmp-npm",
        async = true,
        -- optional - make blink-cmp-npm completions top priority (see `:h blink.cmp`)
        score_offset = 100,
        -- optional - blink-cmp-npm config
        ---@module "blink-cmp-npm"
        ---@type blink-cmp-npm.Options
        opts = {
          ignore = {},
          only_semantic_versions = true,
          only_latest_version = false,
        }
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
