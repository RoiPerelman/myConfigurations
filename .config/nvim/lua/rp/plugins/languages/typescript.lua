_G.lsp_config_servers.ts_ls = {}
_G.lsp_config_servers.eslint = {}

_G.lsp_config_clients_callbacks.ts_ls = function(client, event)
  -- remove format so eslint can do it
  client.server_capabilities.documentFormattingProvider = false
  client.server_capabilities.documentRangeFormattingProvider = false
end

_G.lsp_config_clients_callbacks.eslint = function(client, event)
  -- add eslint format on save
  local organize_imports = function()
    -- unfortunately, code_action is async, so we need to wait for it to finish
    vim.wait(100)
    vim.lsp.buf.code_action({ context = { only = { "source.removeUnusedImports.ts" } }, apply = true })
    vim.wait(100)
    vim.lsp.buf.code_action({ context = { only = { "source.organizeImports.ts" } }, apply = true })
  end
  vim.keymap.set("n", "co", organize_imports, { buffer = event.buf, desc = "[C]ode [O]rganize imports eslint" })
  vim.keymap.set("n", "cf", function()
    vim.cmd("LspEslintFixAll")
  end, { buffer = event.buf, desc = "[C]ode [F]ormat + fix eslint" })
  vim.keymap.set("n", "g=", function()
    organize_imports()
    vim.cmd("LspEslintFixAll")
  end, { buffer = event.buf, desc = "[G]et [=]format + organize + fix eslint" })
  vim.api.nvim_create_autocmd("BufWritePre", {
    buffer = event.buf,
    command = "LspEslintFixAll",
  })
end
