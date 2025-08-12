vim.pack.add({ "https://github.com/L3MON4D3/LuaSnip" })
vim.pack.add({ "https://github.com/rafamadriz/friendly-snippets" })
vim.pack.add({ { src = "https://github.com/saghen/blink.cmp", version = "v1.1.1" } })
vim.pack.add({ "https://github.com/alexandre-abrioux/blink-cmp-npm.nvim" })

require('luasnip.loaders.from_vscode').lazy_load()

require('blink.cmp').setup({
  sources = {
    default = { "lsp", "path", "snippets", "buffer", "npm" },
    providers = {
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
    documentation = { auto_show = true },
    menu = {
      auto_show = true,
      draw = {
        treesitter = { "lsp" },
        columns = { { "kind_icon", "label", "label_description", gap = 1 }, { "kind" } },
      }
    }
  },
  signature = {
    enabled = true
  }
})
