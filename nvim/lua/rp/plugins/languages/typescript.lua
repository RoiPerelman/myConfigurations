vim.list_extend(_G.mason_ensure_installed, { "ts_ls", "eslint" })

return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        -- TODO: check https://github.com/pmizio/typescript-tools.nvim
        ts_ls = {},
        eslint = {},
      },
      setup = {
        ts_ls = function(client, event)
          -- remove format so eslint can do it
          client.server_capabilities.documentFormattingProvider = false
          client.server_capabilities.documentRangeFormattingProvider = false
        end,
        eslint = function(client, event)
          -- add eslint format on save
          local organize_imports = function()
            -- unfortunately, code_action is async, so we need to wait for it to finish
            vim.wait(100)
            vim.lsp.buf.code_action({ context = { only = { "source.removeUnusedImports.ts" } }, apply = true })
            vim.wait(100)
            vim.lsp.buf.code_action({ context = { only = { "source.organizeImports.ts" } }, apply = true })
          end
          vim.keymap.set("n", "co", organize_imports, { buffer = event.buf, desc = "[C]ode [O]rganize imports" })
          vim.keymap.set("n", "cf", function()
            vim.cmd("EslintFixAll")
          end, { buffer = event.buf, desc = "[C]ode [F]ormat + fix" })
          vim.keymap.set("n", "g=", function()
            vim.cmd("EslintFixAll")
          end, { buffer = event.buf, desc = "[G]et [=]format + organize + fix" })
          vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = event.buf,
            command = "EslintFixAll",
          })
        end,
      }
    },
  },
}
