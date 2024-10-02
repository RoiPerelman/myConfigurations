-- LSP servers and clients are able to communicate to each other what features they support.
--  By default, Neovim doesn't support everything that is in the LSP specification.
--  When you add nvim-cmp, luasnip, etc. Neovim now has *more* capabilities.
--  So, we create new capabilities with nvim cmp, and then broadcast that to the servers.
local capabilities = vim.lsp.protocol.make_client_capabilities()
-- capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

local servers = _G.lsp_config_servers

require("mason-lspconfig").setup({
  handlers = {
    function(server_name)
      local server = servers[server_name] or {}
      server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
      require("lspconfig")[server_name].setup(server)
    end,
  },
})

vim.api.nvim_create_autocmd("LspAttach", {
  desc = "this function gets run when an lsp attaches to a particular buffer",
  group = vim.api.nvim_create_augroup("rp-lsp-attach", { clear = true }),
  callback = function(event)
    local map = function(keys, func, desc)
      vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    end
    map("gd", require("telescope.builtin").lsp_definitions, "[G]oto [D]efinition")
    map("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")
    map("gI", require("telescope.builtin").lsp_implementations, "[G]oto [I]mplementation")
    map("gt", require("telescope.builtin").lsp_type_definitions, "[G]oto [T]ype Definition")
    map("gs", require("telescope.builtin").lsp_document_symbols, "[G]oto Document [S]ymbols")
    map("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
    map("<Leader>cn", vim.lsp.buf.rename, "[C]ode Re[N]ame")
    map("<Leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")
    map("K", vim.lsp.buf.hover, "Hover Do[K]umentation")

    map("g=", vim.lsp.buf.format, "[G]et [=]format lsp")
    map("<leader>cf", vim.lsp.buf.format, "[C]ode [F]ormat")

    -- Diagnostics
    vim.diagnostic.config({ virtual_text = false }) -- remove virtual text
    vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Go to previous [D]iagnostic message" })
    vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Go to next [D]iagnostic message" })
    vim.keymap.set("n", "E", vim.diagnostic.open_float, { desc = "Show diagnostic [E]rror messages" })
    -- vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })

    local client = vim.lsp.get_client_by_id(event.data.client_id)

    if client == nil then
      return
    end


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
