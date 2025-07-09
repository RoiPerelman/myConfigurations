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

local function get_ruff_client_id(bufnr)
  for _, client in pairs(vim.lsp.get_active_clients({ bufnr = bufnr })) do
    if client.name == "ruff" then
      return client.id
    end
  end
  return nil
end

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

    local ruff_auto_fix = function()
      local ruff_client_id = get_ruff_client_id(bufnr)
      vim.lsp.buf.code_action({
        context = { only = { "source.fixAll" } },
        apply = true,
        client_id = ruff_client_id,
      })
    end
    vim.api.nvim_buf_create_user_command(bufnr, 'LspRuffAutofix', function()
      ruff_auto_fix()
    end, { desc = "Ruff: Fix all auto-fixable problems" })

    local ruff_organize_imports = function()
      local ruff_client_id = get_ruff_client_id(bufnr)
      vim.lsp.buf.code_action({
        context = { only = { "source.organizeImports" } },
        apply = true,
        client_id = ruff_client_id,
      })
    end
    vim.api.nvim_buf_create_user_command(bufnr, 'LspRuffOrganizeImports', function()
      ruff_organize_imports()
    end, { desc = "Ruff: Organize imports" })

    local organize_imports_and_fix_all = function()
      ruff_organize_imports()
      -- unfortunately, code_action is async, so we need to wait for it to finish
      vim.wait(100)
      ruff_auto_fix()
      vim.wait(100)
      vim.lsp.buf.format({
        filter = function(cli)
          return cli.name == "ruff"
        end,
      })
    end
    vim.keymap.set(
      "n",
      "g=",
      organize_imports_and_fix_all,
      { buffer = bufnr, desc = "[G]et [=]format + organize + fix ruff" }
    )
  end
}
