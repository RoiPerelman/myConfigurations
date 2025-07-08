---@brief
---
--- https://github.com/astral-sh/ruff
---
--- A Language Server Protocol implementation for Ruff, an extremely fast Python linter and code formatter, written in Rust. It can be installed via `pip`.
---
--- ```sh
--- pip install ruff
--- ```
---
--- **Available in Ruff `v0.4.5` in beta and stabilized in Ruff `v0.5.3`.**
---
--- This is the new built-in language server written in Rust. It supports the same feature set as `ruff-lsp`, but with superior performance and no installation required. Note that the `ruff-lsp` server will continue to be maintained until further notice.
---
--- Server settings can be provided via:
---
--- ```lua
--- vim.lsp.config('ruff', {
---   init_options = {
---     settings = {
---       -- Server settings should go here
---     }
---   }
--- })
--- ```
---
--- Refer to the [documentation](https://docs.astral.sh/ruff/editors/) for more details.
return {
  cmd = { 'ruff', 'server' },
  filetypes = { 'python' },
  root_markers = { 'pyproject.toml', 'ruff.toml', '.ruff.toml', '.git' },
  settings = {},
  on_attach = function(client, bufnr)
    -- Disable hover in favor of Pyright
    client.server_capabilities.hoverProvider = false
    -- allow formatting from Ruff
    client.server_capabilities.documentFormattingProvider = true

    vim.api.nvim_buf_create_user_command(bufnr, 'LspRuffAutofix', function()
      vim.lsp.buf.code_action({
        context = { only = { "source.fixAll" } },
        apply = true,
      })
    end, { desc = "Ruff: Fix all auto-fixable problems" })

    vim.api.nvim_buf_create_user_command(bufnr, 'LspRuffOrganizeImports', function()
      vim.lsp.buf.code_action({
        context = { only = { "source.organizeImports" } },
        apply = true,
      })
    end, { desc = "Ruff: Organize imports" })


    local organize_imports_and_fix_all = function()
      vim.lsp.buf.code_action({
        context = { only = { "source.organizeImports" } },
        apply = true,
      })
      -- unfortunately, code_action is async, so we need to wait for it to finish
      vim.wait(100)
      vim.lsp.buf.code_action({
        context = { only = { "source.fixAll" } },
        apply = true,
      })
      vim.wait(100)
      vim.lsp.buf.format()
    end
    vim.keymap.set(
      "n",
      "g=",
      organize_imports_and_fix_all,
      { buffer = bufnr, desc = "[G]et [=]format + organize + fix ruff" }
    )
  end
}
