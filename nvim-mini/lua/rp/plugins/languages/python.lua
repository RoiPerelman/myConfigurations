-- for python we need 2 things
-- pyright lsp (code analysis + lint)
-- ruff lsp (lint + format)

vim.list_extend(_G.mason_ensure_installed, { "pyright", "ruff" })

_G.lsp_config_servers.pyright = {
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
}

_G.lsp_config_servers.ruff = {
  commands = {
    RuffAutofix = {
      function()
        vim.lsp.buf.code_action({
          context = { only = { "source.fixAll" } },
          apply = true,
        })
      end,
      description = "Ruff: Fix all auto-fixable problems",
    },
    RuffOrganizeImports = {
      function()
        vim.lsp.buf.code_action({
          context = { only = { "source.organizeImports" } },
          apply = true,
        })
      end,
      description = "Ruff: Format imports",
    },
  },
}

---@diagnostic disable-next-line: duplicate-set-field
_G.lsp_config_clients_callbacks.pyright = function(client)
  client.server_capabilities.codeActionProvider = false
end

---@diagnostic disable-next-line: duplicate-set-field
_G.lsp_config_clients_callbacks.ruff = function(client, event)
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
    { buffer = event.buf, desc = "[G]et [=]format + organize + fix ruff" }
  )
  -- vim.api.nvim_create_autocmd("BufWritePre", {
  --   buffer = event.buf,
  --   callback = organize_imports_and_fix_all,
  -- })
end
