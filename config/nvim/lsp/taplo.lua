---@brief
---
--- https://taplo.tamasfe.dev/cli/usage/language-server.html
---
--- Language server for Taplo, a TOML toolkit.
---
--- `taplo-cli` can be installed via `cargo`:
--- ```sh
--- cargo install --features lsp --locked taplo-cli
--- ```

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = vim.tbl_deep_extend("force", capabilities,
  require("blink.cmp").get_lsp_capabilities() or {}
)

return {
  cmd = { 'taplo', 'lsp', 'stdio' },
  filetypes = { 'toml' },
  root_markers = { '.taplo.toml', 'taplo.toml', '.git' },
  capabilities = capabilities,
  init_options = {
    configuration = {
      schema = {
        enabled = true,
        repository = true,
        links = true,
      }
    }
  },
  settings = {
    -- Taplo configuration
    taplo = {
      schema = {
        enabled = true,
        associations = {
          -- Python related schemas
          ["pyproject.toml"] = "https://json.schemastore.org/pyproject.json",
          ["poetry.toml"] = "https://json.schemastore.org/poetry.json",
          ["pdm.toml"] = "https://raw.githubusercontent.com/pdm-project/pdm/main/schema/pdm-schema.json",

          -- Rust related schemas (though Cargo.toml is automatically handled)
          ["Cargo.toml"] = "https://json.schemastore.org/cargo.json",
          [".clippy.toml"] = "https://json.schemastore.org/clippy.json",

          -- Configuration files
          ["mkdocs.yml"] = "https://json.schemastore.org/mkdocs.json",
          [".prettierrc.toml"] = "https://json.schemastore.org/prettierrc.json",
          ["renovate.toml"] = "https://docs.renovatebot.com/renovate-schema.json",
          ["ecosystem.config.toml"] = "https://json.schemastore.org/pm2-ecosystem.json",

          -- Generic schemas
          ["package.toml"] = "https://json.schemastore.org/package.json",
          ["config.toml"] = "https://json.schemastore.org/config.json",
        },
      },
      formatter = {
        enabled = true,
        formatArrayTrailingComma = true,
        formatIndentTables = true,
      },
      diagnostics = {
        enabled = true,
        -- Disable specific diagnostics if needed
        -- disabled = { "incorrect-type" },
      },
    },
  },
}
