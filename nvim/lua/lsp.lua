local nvim_lsp = require('lspconfig')

USER = vim.fn.expand('$USER')

local lspinstall_path = vim.fn.stdpath('data') .. "/lspinstall"

local custom_lsp_attach = function(client)
    if client.config.flags then
      client.config.flags.allow_incremental_sync = true
    end
    client.resolved_capabilities.document_formatting = false

    vim.api.nvim_buf_set_keymap(0, 'n', 'gh', '<cmd>lua vim.lsp.buf.hover()<CR>', {noremap = true})
    vim.api.nvim_buf_set_keymap(0, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', {noremap = true})
    vim.api.nvim_buf_set_keymap(0, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', {noremap = true})
    vim.api.nvim_buf_set_keymap(0, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', {noremap = true})

    -- Use LSP as the handler for omnifunc.
    vim.bo.omnifunc = 'v:lua.vim.lsp.omnifunc'
end

-- lua language server
local sumneko_binary = lspinstall_path .. "/lua/sumneko-lua-language-server"

local luadev = require'lua-dev'.setup({
  -- add any options here, or leave empty to use the default settings
  lspconfig = {
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
  },
})
nvim_lsp.sumneko_lua.setup(luadev)

-- typescript language server
local typescript_language_server_binary = lspinstall_path .. "/typescript/node_modules/.bin/typescript-language-server"
nvim_lsp.tsserver.setup({
  cmd = {typescript_language_server_binary, "--stdio"},
  on_attach = custom_lsp_attach,
  -- TODO understand handlers better - maybe underline the specific word/ show and don't show virtual text on command
  -- and dont change line color?
  -- handlers = {
  --   ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  --     virtual_text = true,
  --     signs = true,
  --     underline = true,
  --     update_in_insert = true
  --   })
  -- }
})

-- python language server
local pyright_language_server_binary = lspinstall_path .. "/python/node_modules/.bin/pyright-langserver"
nvim_lsp.pyright.setup {
  on_attach = custom_lsp_attach,
  cmd = {pyright_language_server_binary , "--stdio"},
  settings = {
    python = {
      analysis = {
        typeCheckingMode = "off",
        useLibraryCodeForTypes = false,
      }
    }
  },
  -- handlers = {
  --   ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  --     virtual_text = false,
  --     signs =false,
  --     underline =false,
  --     update_in_insert =false
  --   })
  -- }
}

-- efm language server (for linting and formatting!)
local efm_on_attach = function(client)
  if client.resolved_capabilities.document_formatting then
    vim.cmd [[augroup lsp_formatting]]
    vim.cmd [[autocmd!]]
    vim.cmd [[autocmd BufWritePre <buffer> :lua vim.lsp.buf.formatting_sync()]]
    vim.cmd [[augroup END]]
  end
end

local eslint = {
  lintCommand = "eslint_d -f unix --stdin --stdin-filename ${INPUT}",
  lintStdin = true,
  lintFormats = {"%f:%l:%c: %m"},
  lintIgnoreExitCode = true,
  formatCommand = "eslint_d --fix-to-stdout --stdin --stdin-filename=${INPUT}",
  formatStdin = true
}

local pep8 = {
}

local efm_language_server = lspinstall_path .. "/efm/efm-langserver"
nvim_lsp.efm.setup({
  cmd = {efm_language_server, '-logfile', '/tmp/efm.log', '-loglevel', '5'},
  on_attach = efm_on_attach,
  init_options = {
    documentFormatting = true,
    hover = true,
    document_symbols = true,
    codeAction = true,
    completion = true,
  },
  filetypes = {'javascript', 'javascriptreact', 'typescript', 'typescriptreact'},
  settings = {
    rootMarkers = {'.git/'},
    languages = {
      javascript = {eslint},
      javascriptreact = {eslint},
      typescript = {eslint},
      typescriptreact = {eslint},
    },
  },
}

-- local function eslint_config_exists()
--   local eslintrc = vim.fn.glob(".eslintrc*", 0, 1)

--   if not vim.tbl_isempty(eslintrc) then
--     return true
--   end

--   if vim.fn.filereadable("package.json") then
--     if vim.fn.json_decode(vim.fn.readfile("package.json"))["eslintConfig"] then
--       return true
--     end
--   end

--   return false
-- end

)
