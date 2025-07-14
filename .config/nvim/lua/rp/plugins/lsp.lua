---@brief
--- to have lsp we have a couple of moving components:
---
--- 1. lsp server (e.g. lua_ls, pyright, etc.) - install by mason.lua
---    mason installs the lsp servers and loads them into path so nvim can find and use them
--- 2. lsp configuration - per language server add a file in `lsp/` directory with vim.lsp.config
---    base configs can be copies from lsp-config `https://github.com/neovim/nvim-lspconfig` `lsp/` directory
--- 3. enable lsp server - this allows us to start lsp servers with the config we put in the `lsp/` directory
---
--- It is our job to make sure the config is right. We add whatever keymaps/ commands and capabilities we want
--- client capabilities are set in `lsp/` directory as part of config
---
--- Helpful commands:
--- To know server capabilities `:= vim.lsp.get_active_clients()[1].server_capabilities`

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
