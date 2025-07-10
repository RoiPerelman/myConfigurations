-- LSP servers and clients are able to communicate to each other what features they support.
--  By default, Neovim doesn't support everything that is in the LSP specification.
-- local capabilities = vim.lsp.protocol.make_client_capabilities()

-- local servers = _G.lsp_config_servers

-- require("mason-lspconfig").setup({
--   handlers = {
--     function(server_name)
--       local server = servers[server_name] or {}
--       server.capabilities = vim.tbl_deep_extend("force", {}, capabilities,
--         -- add blink capabilities
--         require('blink.cmp').get_lsp_capabilities(server.capabilities) or {})
--       require("lspconfig")[server_name].setup(server)
--     end,
--   },
-- })

-- help lsp stuff for lua
require("lazydev").setup({
  library = {
    -- See the configuration section for more details
    -- Load luvit types when the `vim.uv` word is found
    { path = "luvit-meta/library", words = { "vim%.uv" } },
  },
})

vim.lsp.enable({
  "lua_ls",
  "pyright",
  "ruff",
  "ts_ls",
  "eslint",
  "bashls",
  "jsonls",
})

vim.api.nvim_create_autocmd("LspAttach", {
  desc = "this function gets run when an lsp attaches to a particular buffer",
  group = vim.api.nvim_create_augroup("rp-lsp-attach", { clear = true }),
  callback = function(event)
    local map = function(keys, func, desc)
      vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    end
    map("K", vim.lsp.buf.hover, "Hover Do[K]umentation")

    map("g=", vim.lsp.buf.format, "[G]et [=]format lsp")
    map("<leader>cf", vim.lsp.buf.format, "[C]ode [F]ormat")

    -- Diagnostics
    vim.diagnostic.config({ virtual_text = false }) -- remove virtual text
    vim.keymap.set("n", "E", vim.diagnostic.open_float, { desc = "Show diagnostic [E]rror messages" })
    -- vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })

    local client = vim.lsp.get_client_by_id(event.data.client_id)

    if client == nil then
      return
    end

    -- nvim v0.11 supports autocompletion - C-s to show signature
    -- not using as I am using blink.cmp which takes the lsp and shows it itself
    -- if client:supports_method('textDocument/completion') then
    --   vim.lsp.completion.enable(true, client.id, event.buf, { autotrigger = true })
    -- end

    -- To know server capabilities, use (for example):
    -- :lua =vim.lsp.get_active_clients()[1].server_capabilities
    if _G.lsp_config_clients_callbacks[client.name] then
      _G.lsp_config_clients_callbacks[client.name](client, event)
    end

    -- The following two autocommands are used to highlight references of the
    -- word under your cursor when your cursor rests there for a little while.
    -- When you move your cursor, the highlights will be cleared (the second autocommand).
    if client and client.server_capabilities.documentHighlightProvider then
      vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
        buffer = event.buf,
        callback = vim.lsp.buf.document_highlight,
      })
      vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
        buffer = event.buf,
        callback = vim.lsp.buf.clear_references,
      })
    end
  end,
})
