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
--- ctrl-]          -> go to definition
--- gq              -> format selected text or text object
--- K               -> display documentation of the symbol under the cursor
--- ctrl-x + ctrl-o -> in insert mode, trigger code completion
--- grn        -> renames all references of the symbol under the cursor
--- gra        -> list code actions available in the line under the cursor
--- grr        -> lists all the references of the symbol under the cursor
--- gri        -> lists all the implementations for the symbol under the cursor
--- gO         -> lists all symbols in the current buffer
--- ctrl-s     -> in insert mode, display function signature under the cursor
--- [d         -> jump to previous diagnostic in the current buffer
--- ]d         -> jump to next diagnostic in the current buffer
--- ctrl-w + d -> show error/warning message in the line under the cursor
--- := vim.lsp.get_active_clients()[1].server_capabilities -> To know server capabilities

vim.lsp.enable({
  "lua_ls",
  "pyright",
  "ruff",
  "ts_ls",
  "eslint",
  "bashls",
  "jsonls",
  "taplo",
  "yamlls",
  "docker",
  "marksman",
})

vim.api.nvim_create_autocmd("LspAttach", {
  desc = "this function gets run when an lsp attaches to a particular buffer",
  group = vim.api.nvim_create_augroup("rp-lsp-attach", { clear = true }),
  callback = function(event)
    -- Diagnostics
    vim.diagnostic.config({ virtual_text = false }) -- remove virtual text
    vim.keymap.set("n", "E", vim.diagnostic.open_float, { desc = "Show diagnostic [E]rror messages" })

    local client = vim.lsp.get_client_by_id(event.data.client_id)

    if client == nil then
      return
    end

    -- NOTE: not being used as blink.nvim is used for autocompletion
    -- nvim v0.11 supports autocompletion
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
