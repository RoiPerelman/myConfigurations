USER = vim.fn.expand('$USER')

local lspinstall_path = vim.fn.stdpath('data') .. "/lspinstall"

-- this is a bash script calling the binary with a path to main.lua
local sumneko_binary = lspinstall_path .. "/lua/sumneko-lua-language-server"

local custom_lsp_attach = function(client)

    Roiprint = function()
      print('roiroi')
      print(vim.inspect(client))
    end

    vim.api.nvim_buf_set_keymap(0, 'n', 'gh', '<cmd>lua vim.lsp.buf.hover()<CR>', {noremap = true})
    vim.api.nvim_buf_set_keymap(0, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', {noremap = true})
    vim.api.nvim_buf_set_keymap(0, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', {noremap = true})
    vim.api.nvim_buf_set_keymap(0, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', {noremap = true})
    vim.api.nvim_buf_set_keymap(0, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', {noremap = true})
    vim.api.nvim_buf_set_keymap(0, 'n', 'gt', '<cmd>lua Roiprint()<CR>', {noremap = true})

    -- Use LSP as the handler for omnifunc.
    -- See `:help omnifunc` and `:help ins-completion` for more information.
    -- vim.api.nvim_buf_set_option(0, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
    vim.bo.omnifunc = 'v:lua.vim.lsp.omnifunc'
    -- For plugins with an `on_attach` callback, call them here. For example:
    --- Set autocommands conditional on server_capabilities
    -- require('completion').on_attach()

    -- TODO doesn't really work plus - do i want this?
    -- document highlight on hover (same word)
    -- if client.resolved_capabilities.document_highlight then
    --     vim.api.nvim_exec(
    --       [[
    --         hi LspReferenceRead cterm=bold ctermbg=red guibg=#464646
    --         hi LspReferenceText cterm=bold ctermbg=red guibg=#464646
    --         hi LspReferenceWrite cterm=bold ctermbg=red guibg=#464646
    --         augroup lsp_document_highlight
    --           autocmd! * <buffer>
    --           autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
    --           autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
    --         augroup END
    --       ]],
    --       false
    --     )
    -- end
end

-- lua language server
require'lspconfig'.sumneko_lua.setup {
    on_attach = custom_lsp_attach,
    cmd = {sumneko_binary};
    settings = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = 'LuaJIT',
                -- Setup your lua path
                path = vim.split(package.path, ';')
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = {'vim'}
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = {
                  [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                  [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true
                },
            }
        }
    }
}

-- typescript language server
local typescript_language_server_binary = lspinstall_path .. "/typescript/node_modules/.bin/typescript-language-server"
require'lspconfig'.tsserver.setup {
    on_attach = custom_lsp_attach,
    cmd = {typescript_language_server_binary, "--stdio"},
    -- defaults
    -- cmd = { "typescript-language-server", "--stdio" }
    -- filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx" },
    -- root_dir = root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git")
}

-- python language server
local pyright_language_server_binary = lspinstall_path .. "/python/node_modules/.bin/pyright-langserver"
require'lspconfig'.pyright.setup {
    on_attach = custom_lsp_attach,
    cmd = {pyright_language_server_binary , "--stdio"},
}


-- tsserver/web javascript react, vue, json, html, css, yaml
local prettier = {formatCommand = "prettier --stdin-filepath ${INPUT}", formatStdin = true}
-- You can look for project scope Prettier and Eslint with e.g. vim.fn.glob("node_modules/.bin/prettier") etc. If it is not found revert to global Prettier where needed.
-- local prettier = {formatCommand = "./node_modules/.bin/prettier --stdin-filepath ${INPUT}", formatStdin = true}

local eslint = {
    lintCommand = "./node_modules/.bin/eslint -f unix --stdin --stdin-filename ${INPUT}",
    lintIgnoreExitCode = true,
    lintStdin = true,
    lintFormats = {"%f:%l:%c: %m"},
    formatCommand = "./node_modules/.bin/eslint --fix-to-stdout --stdin --stdin-filename=${INPUT}",
    formatStdin = true
}

-- efm language server (for linting and formatting!)
local efm_language_server = lspinstall_path .. "/efm/efm-langserver"
require'lspconfig'.efm.setup {
  cmd = {efm_language_server},
  init_options = {documentFormatting = true, codeAction = false},
  -- filetypes = {"lua", "python", "javascriptreact", "javascript", "typescript","typescriptreact","sh", "html", "css", "json", "yaml", "markdown", "vue"},
  filetypes = {"javascriptreact", "javascript", "typescript","typescriptreact"},
  settings = {
    rootMarkers = {".git/"},
    languages = {
      -- python = python_arguments,
      -- lua = lua_arguments,
      -- sh = sh_arguments,
      javascript = eslint,
      javascriptreact = eslint,
      typescript = eslint,
      typescriptreact = eslint,
      -- html = {prettier},
      -- css = {prettier},
      -- json = {prettier},
      -- yaml = {prettier},
      -- markdown = {markdownPandocFormat}
      -- markdown = {markdownPandocFormat, markdownlint},
    }
  }
}
