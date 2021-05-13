USER = vim.fn.expand('$USER')

-- vim.fn.stdpath('data') expands to /home/roiperelman/.local/share/nvim
local sumneko_root_path = vim.fn.stdpath('data') .. "/lspinstall/lua"
local sumneko_binary = sumneko_root_path .. "/sumneko-lua-language-server"

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
    cmd = { sumneko_binary },
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
