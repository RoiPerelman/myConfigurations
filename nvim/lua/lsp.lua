USER = vim.fn.expand('$USER')

local lspinstall_path = vim.fn.stdpath('data') .. "/lspinstall"

local custom_lsp_attach = function(client)

    Roiprint = function()
      print('roiroi')
      print(vim.inspect(client))
    end

    if client.config.flags then
      print('ROIROI', client.config.flags.allow_incremental_sync )
      client.config.flags.allow_incremental_sync = true
    end
    client.resolved_capabilities.document_formatting = false

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
require'lspconfig'.sumneko_lua.setup(luadev)

-- typescript language server
-- local typescript_language_server_binary = lspinstall_path .. "/typescript/node_modules/.bin/typescript-language-server"
-- require'lspconfig'.tsserver.setup {
--   on_attach = custom_lsp_attach,
--   cmd = {typescript_language_server_binary, "--stdio"},
--   root_dir = require'lspconfig/util'.root_pattern("package.json", ".git"),
--   settings = {documentFormatting = false},
--   handlers = {
--     ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
--       virtual_text = true,
--       signs = true,
--       underline = true,
--       update_in_insert = true
--     })
--   }
--   -- defaults
--   -- cmd = { "typescript-language-server", "--stdio" }
--   -- filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx" },
--   -- root_dir = root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git")
-- }

-- python language server
local pyright_language_server_binary = lspinstall_path .. "/python/node_modules/.bin/pyright-langserver"
require'lspconfig'.pyright.setup {
  on_attach = custom_lsp_attach,
  cmd = {pyright_language_server_binary , "--stdio"},
  settings = {
    python = {
      analysis = {
        useLibraryCodeForTypes = false
      }
    }
  }
}


-- tsserver/web javascript react, vue, json, html, css, yaml
-- local prettier = {formatCommand = "prettier --stdin-filepath ${INPUT}", formatStdin = true}
-- You can look for project scope Prettier and Eslint with e.g. vim.fn.glob("node_modules/.bin/prettier") etc. If it is not found revert to global Prettier where needed.
-- local prettier = {formatCommand = "./node_modules/.bin/prettier --stdin-filepath ${INPUT}", formatStdin = true}

-- local eslint = {
--   lintCommand = "eslint_d -f unix --stdin --stdin-filename ${INPUT}",
--   lintStdin = true,
--   lintFormats = {"%f:%l:%c: %m"},
--   lintIgnoreExitCode = true,
--   -- formatCommand = "eslint_d --fix-to-stdout --stdin --stdin-filename=${INPUT}",
--   formatCommand = "prettier --stdin-filepath ${INPUT}",
--   formatStdin = true
-- }

-- local eslint = {
--   lintCommand = "./node_modules/.bin/eslint -f unix --stdin --stdin-filename ${INPUT}",
--   lintStdin = true,
--   lintFormats = {"%f:%l:%c: %m"},
--   lintIgnoreExitCode = true,
--   formatCommand = "./node_modules/.bin/eslint --fix-to-stdout --stdin --stdin-filename=${INPUT}",
--   formatStdin = true,
-- }

-- local prettier = {
--   formatCommand = (
--     function()
--       if not vim.fn.empty(vim.fn.glob(vim.loop.cwd() .. '/.prettierrc')) then
--         return "./node_modules/.bin/prettier --config ./.prettierrc"
--       else
--         return "prettier --config ~/.config/nvim/.prettierrc"
--       end
--     end
--   )()
-- }

-- efm language server (for linting and formatting!)

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

-- local efm_language_server = lspinstall_path .. "/efm/efm-langserver"
-- require'lspconfig'.efm.setup {
--   cmd = {efm_language_server},
--   -- root_dir = function()
--   --   if not eslint_config_exists() then
--   --     print('ROIROI huhuhuhu')
--   --     return nil
--   --   end
--   --   print('ROIROI what', vim.fn.getcwd())
--   --   return vim.fn.getcwd()
--   -- end,
--   -- init_options = {documentFormatting = true, hover= false, documentSymbol= false, codeAction = false, completion= false},
--   init_options = {documentFormatting = true},
--   -- filetypes = {"lua", "python", "javascriptreact", "javascript", "typescript","typescriptreact","sh", "html", "css", "json", "yaml", "markdown", "vue"},
--   filetypes = {"javascript", "javascriptreact", "typescript","typescriptreact"},
--   settings = {
--     rootMarkers = {".git/"},
--     languages = {
--       -- python = python_arguments,
--       -- lua = lua_arguments,
--       -- sh = sh_arguments,
--       javascript = {eslint},
--       javascriptreact = {eslint},
--       typescript = {prettier},
--       typescriptreact = {prettier},
--       -- html = {prettier},
--       -- css = {prettier},
--       -- json = {prettier},
--       -- yaml = {prettier},
--       -- markdown = {markdownPandocFormat}
--       -- markdown = {markdownPandocFormat, markdownlint},
--     }
--   }
-- }
--

local nvim_lsp = require('lspconfig')

local on_attach = function(client)
  -- if client.resolved_capabilities.document_formatting then
  --   vim.cmd [[augroup lsp_formatting]]
  --   vim.cmd [[autocmd!]]
  --   vim.cmd [[autocmd BufWritePre <buffer> :lua vim.lsp.buf.formatting_sync()]]
  --   vim.cmd [[augroup END]]
  -- end
end

local efm_formatters = {
  prettier = {
    formatCommand = './node_modules/.bin/prettier',
    rootMarkers = {'package.json'},
  },
  eslint = {
    lintCommand = "eslint_d -f unix --stdin --stdin-filename ${INPUT}",
    lintStdin = true,
    lintFormats = {"%f:%l:%c: %m"},
    lintIgnoreExitCode = true,
    formatCommand = "eslint_d --fix-to-stdout --stdin --stdin-filename=${INPUT}",
    formatStdin = true
  }
}

local eslint = {
  lintCommand = "eslint_d -f unix --stdin --stdin-filename ${INPUT}",
  lintStdin = true,
  lintFormats = {"%f:%l:%c: %m"},
  lintIgnoreExitCode = true,
  formatCommand = "eslint_d --fix-to-stdout --stdin --stdin-filename=${INPUT}",
  formatStdin = true
}

local efm_language_server = lspinstall_path .. "/efm/efm-langserver"
nvim_lsp.efm.setup({
  on_attach = on_attach,
  cmd = {efm_language_server, '-logfile', '/tmp/efm.log', '-loglevel', '5'},
  init_options = {
    documentFormatting = true,
  },
  filetypes = {'typescript', 'typescriptreact'},
  settings = {
    rootMarkers = {'.git/'},
    languages = {
      typescript = {eslint},
      typescriptreact = {eslint},
    },
  },
})

local typescript_language_server_binary = lspinstall_path .. "/typescript/node_modules/.bin/typescript-language-server"
nvim_lsp.tsserver.setup({
  cmd = {typescript_language_server_binary, "--stdio"},
  on_attach = function(client)
    client.resolved_capabilities.document_formatting = false
    on_attach(client)
  end,
})
