vim.list_extend(_G.mason_ensure_installed, { "lua-language-server", "stylua" })

_G.lsp_config_servers.lua_ls = {
  settings = {
    Lua = {
      runtime = { version = "LuaJIT" },
      workspace = {
        checkThirdParty = false,
        -- Tells lua_ls where to find all the Lua files that you have loaded
        -- for your neovim configuration.
        library = {
          "${3rd}/luv/library",
          unpack(vim.api.nvim_get_runtime_file("", true)),
        },
        -- If lua_ls is really slow on your computer, you can try this instead:
        -- library = { vim.env.VIMRUNTIME },
      },
      completion = {
        callSnippet = "Replace",
      },
      telemetry = { enable = false },
      diagnostics = {
        globals = { "_G", "mason_ensure_installed" }, -- Add "mason_ensure_installed" here
        disable = { "missing-fields" },  -- Other disabled diagnostics
      },
    },
  },
}

_G.lsp_config_clients_callbacks.lua_ls = function(client, event)
  print('ROIROI im here')
  vim.api.nvim_create_autocmd("BufWritePre", {
    buffer = event.buf,
    callback = function()
      vim.lsp.buf.format()
    end,
  })
end
