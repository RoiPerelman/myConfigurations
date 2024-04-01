-- LSP Configuration & Plugins
return {
	"neovim/nvim-lspconfig", -- add client configurations to start talking to server
	dependencies = {
		"williamboman/mason.nvim", -- manages lsp, dap, linter and formatter servers
		"williamboman/mason-lspconfig.nvim", -- help setup mason installed servers with lspconfig
		"WhoIsSethDaniel/mason-tool-installer.nvim", -- help automatically install servers

		{ "j-hui/fidget.nvim", opts = {} }, -- Useful status updates for LSP.

		{ "folke/neodev.nvim", opts = {} }, -- configures Lua LSP for neovim config
	},
	config = function()
		-- This function gets run when an LSP attaches to a particular buffer.
		vim.api.nvim_create_autocmd("LspAttach", {
			group = vim.api.nvim_create_augroup("kickstart-lsp-attach", { clear = true }),
			callback = function(event)
				-- remove virtual text
				vim.diagnostic.config({ virtual_text = false })

				local map = function(keys, func, desc)
					vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
				end
				map("gd", require("telescope.builtin").lsp_definitions, "[G]oto [D]efinition")
				map("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")
				map("gI", require("telescope.builtin").lsp_implementations, "[G]oto [I]mplementation")

				-- Jump to the type of the word under your cursor.
				--  Useful when you're not sure what type a variable is and you want to see
				--  the definition of its *type*, not where it was *defined*.
				map("gt", require("telescope.builtin").lsp_type_definitions, "[G]oto [T]ype Definition")

				-- Fuzzy find all the symbols in your current document.
				--  Symbols are things like variables, functions, types, etc.
				map("gs", require("telescope.builtin").lsp_document_symbols, "[G]oto Document [S]ymbols")

				-- Fuzzy find all the symbols in your current workspace.
				--  Similar to document symbols, except searches over your entire project.
				map("gw", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[G]oto [W]orkspace Symbols")

				-- Rename the variable under your cursor.
				--  Most Language Servers support renaming across files, etc.
				map("gn", vim.lsp.buf.rename, "[G]et Re[N]ame")

				-- Execute a code action, usually your cursor needs to be on top of an error
				-- or a suggestion from your LSP for this to activate.
				map("gc", vim.lsp.buf.code_action, "[G]et [C]ode Action")

				map("K", vim.lsp.buf.hover, "Hover Documentation")

				-- WARN: This is not Goto Definition, this is Goto Declaration.
				--  For example, in C this would take you to the header.
				map("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")

				-- The following two autocommands are used to highlight references of the
				-- word under your cursor when your cursor rests there for a little while.
				-- When you move your cursor, the highlights will be cleared (the second autocommand).
				local client = vim.lsp.get_client_by_id(event.data.client_id)
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

				-- specific client fixes
				if client.name == "tsserver" then
					-- remove format so eslint can do it
					client.server_capabilities.documentFormattingProvider = false
					client.server_capabilities.documentRangeFormattingProvider = false
				end
				if client.name == "eslint" then
					-- add eslint format on save
					vim.api.nvim_create_autocmd("BufWritePre", {
						buffer = event.buf,
						command = "EslintFixAll",
					})
				end
			end,
		})

		-- LSP servers and clients are able to communicate to each other what features they support.
		--  By default, Neovim doesn't support everything that is in the LSP specification.
		--  When you add nvim-cmp, luasnip, etc. Neovim now has *more* capabilities.
		--  So, we create new capabilities with nvim cmp, and then broadcast that to the servers.
		local capabilities = vim.lsp.protocol.make_client_capabilities()
		capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

		--  Add any additional override configuration in the following tables. Available keys are:
		--  - cmd (table): Override the default command used to start the server
		--  - filetypes (table): Override the default list of associated filetypes for the server
		--  - capabilities (table): Override fields in capabilities. Can be used to disable certain LSP features.
		--  - settings (table): Override the default settings passed when initializing the server.
		local servers = {
			-- TODO: check https://github.com/pmizio/typescript-tools.nvim
			tsserver = {},
			pyright = {
				settings = {
					python = {
						analysis = {
							typeCheckingMode = "off",
						},
					},
				},
			},
			lua_ls = {
				settings = {
					Lua = {
						completion = {
							callSnippet = "Replace",
						},
						-- You can toggle below to ignore Lua_LS's noisy `missing-fields` warnings
						-- diagnostics = { disable = { 'missing-fields' } },
					},
				},
			},
		}

		--  To check the current status of installed tools :Mason
		require("mason").setup()

		-- Add other tools that you want Mason to install automatically for you
		local ensure_installed = vim.tbl_keys(servers or {})
		vim.list_extend(ensure_installed, {
			"eslint-lsp", -- used to lint and format along tsserver
			"stylua", -- Used to format Lua code
			"isort",
			"black",
		})
		-- Automatically install LSPs and related tools to stdpath for Neovim
		require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

		require("mason-lspconfig").setup({
			handlers = {
				function(server_name)
					local server = servers[server_name] or {}
					server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
					require("lspconfig")[server_name].setup(server)
				end,
			},
		})
	end,
}
