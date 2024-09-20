vim.list_extend(_G.mason_ensure_installed, { "pyright", "ruff" })

return {
  -- for python we need 2 things
  -- pyright lsp (code analysis + lint)
  -- ruff lsp (lint + format)
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        pyright = {
          settings = {
            pyright = {
              disableOrganizeImports = true, -- Use Ruff's import organizer
            },
            python = {
              analysis = {
                typeCheckingMode = "off",
              },
            },
          },
        },
        ruff = {
          commands = {
            RuffAutofix = {
              function()
                vim.lsp.buf.execute_command({
                  command = "ruff.applyAutofix",
                  arguments = {
                    { uri = vim.uri_from_bufnr(0) },
                  },
                })
              end,
              description = "Ruff: Fix all auto-fixable problems",
            },
            RuffOrganizeImports = {
              function()
                vim.lsp.buf.execute_command({
                  command = "ruff.applyOrganizeImports",
                  arguments = {
                    { uri = vim.uri_from_bufnr(0) },
                  },
                })
              end,
              description = "Ruff: Format imports",
            },
          },
        },
        -- option instead of pyright
        -- pylsp = {
        --   settings = {
        --     pylsp = {
        --       plugins = {
        --         pyflakes = { enabled = false },
        --         pycodestyle = { enabled = false },
        --         autopep8 = { enabled = false },
        --         yapf = { enabled = false },
        --         mccabe = { enabled = false },
        --         pylsp_mypy = { enabled = false },
        --         pylsp_black = { enabled = false },
        --         pylsp_isort = { enabled = false },
        --       },
        --     },
        --   },
        -- },
      },
      setup = {
        pyright = function(client, event)
          client.server_capabilities.codeActionProvider = false
        end,
        ruff = function(client, event)
          -- Disable hover in favor of Pyright
          client.server_capabilities.hoverProvider = false
          -- Disable auto formatting using a command in my function
          client.server_capabilities.documentFormattingProvider = true

          local organize_imports_and_fix_all = function()
            vim.lsp.buf.code_action({
              context = { only = { "source.organizeImports" } },
              apply = true,
            })
            -- unfortunately, code_action is async, so we need to wait for it to finish
            vim.wait(100)
            vim.lsp.buf.code_action({
              context = { only = { "source.fixAll" } },
              apply = true,
            })
            vim.wait(100)
            vim.lsp.buf.format()
          end
          vim.keymap.set(
            "n",
            "g=",
            organize_imports_and_fix_all,
            { buffer = event.buf, desc = "[G]et [=]format + organize + fix" }
          )
          -- vim.api.nvim_create_autocmd("BufWritePre", {
          --   buffer = event.buf,
          --   callback = organize_imports_and_fix_all,
          -- })
        end,
      }
    },
  },
}
