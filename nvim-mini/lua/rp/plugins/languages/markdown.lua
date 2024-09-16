vim.list_extend(_G.mason_ensure_installed, { "marksman", "markdownlint-cli2", "markdown-toc" })

_G.lsp_config_servers.marksman = {}
_G.conform_formatters["markdown-toc"] = {
  condition = function()
    for _, line in ipairs(vim.api.nvim_buf_get_lines(ctx.buf, 0, -1, false)) do
      if line:find("<!%-%- toc %-%->") then
        return true
      end
    end
  end,
}

_G.conform_formatters["markdownlint-cli2"] = {
  condition = function(_, ctx)
    local diag = vim.tbl_filter(function(d)
      return d.source == "markdownlint"
    end, vim.diagnostic.get(ctx.buf))
    return #diag > 0
  end,
}

_G.conform_formatters_by_ft["markdown"] = { "prettier", "markdownlint-cli2", "markdown-toc" }
_G.conform_formatters_by_ft["markdown.mdx"] = { "prettier", "markdownlint-cli2", "markdown-toc" }

_G.lint_linters_by_ft["markdown"] = { "markdownlint-cli2" }

