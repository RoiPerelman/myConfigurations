USER = vim.fn.expand('$USER')

local lspinstall_path = vim.fn.stdpath('data') .. "/lspinstall"

-- this is a bash script calling the binary with a path to main.lua
local sumneko_binary = lspinstall_path .. "lua/sumneko-lua-language-server"

local custom_lsp_attach = function(client)
    -- print('roiroi')
    -- print(vim.inspect(client))
    -- See `:help nvim_buf_set_keymap()` for more information
    vim.api.nvim_buf_set_keymap(0, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', {noremap = true})
    vim.api.nvim_buf_set_keymap(0, 'n', 'D', '<cmd>lua vim.lsp.buf.definition()<CR>', {noremap = true})
    -- ... and other keymappings for LSP
    -- Use LSP as the handler for omnifunc.
    --    See `:help omnifunc` and `:help ins-completion` for more information.
    vim.api.nvim_buf_set_option(0, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
    -- For plugins with an `on_attach` callback, call them here. For example:
    -- require('completion').on_attach()
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

local typescript_language_server_binary = lspinstall_path .. "/typescript/node_modules/.bin/typescript-language-server"
require'lspconfig'.tsserver.setup{
    on_attach = custom_lsp_attach,
    cmd = {typescript_language_server_binary, "--stdio"},
    -- defaults
    -- cmd = { "typescript-language-server", "--stdio" }
    -- filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx" },
    -- root_dir = root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git")
}

