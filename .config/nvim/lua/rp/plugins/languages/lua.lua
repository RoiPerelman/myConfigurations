vim.list_extend(_G.mason_ensure_installed, { "lua-language-server" })

require("lazydev").setup({
  library = {
    -- See the configuration section for more details
    -- Load luvit types when the `vim.uv` word is found
    { path = "luvit-meta/library", words = { "vim%.uv" } },
  },
})

_G.lsp_config_servers.lua_ls = {
  settings = {
    Lua = {
      runtime = { version = "LuaJIT" },
      workspace = {
        checkThirdParty = false,
        -- Tells lua_ls where to find all the Lua files that you have loaded
        -- for your neovim configuration.
        -- library = {
        --   "${3rd}/luv/library",
        --   unpack(vim.api.nvim_get_runtime_file("", true)),
        -- },
        -- If lua_ls is really slow on your computer, you can try this instead:
        -- library = { vim.env.VIMRUNTIME },
      },
      codeLens = {
        enable = true,
      },
      completion = {
        callSnippet = "Replace",
      },
      doc = {
        privateName = { "^_" },
      },
      hint = {
        enable = true,
        setType = false,
        paramType = true,
        paramName = "Disable",
        semicolon = "Disable",
        arrayIndex = "Disable",
      },
      telemetry = { enable = false },
      diagnostics = {
        globals = { "_G", "mason_ensure_installed", "MiniDeps" }, -- Add "mason_ensure_installed" here
        disable = { "missing-fields" },                           -- Other disabled diagnostics
      },
    },
  },
}

_G.lsp_config_clients_callbacks.lua_ls = function(client, event)
  vim.api.nvim_create_autocmd("BufWritePre", {
    buffer = event.buf,
    callback = function()
      vim.lsp.buf.format()
    end,
  })
end
