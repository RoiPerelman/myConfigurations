vim.pack.add({ "https://github.com/stevearc/conform.nvim" })

local format_on_save_opts = {
  -- lsp_fallback = true,
  async = false,
  timeout_ms = 1000,
}

local formatters_by_ft = {
  sh = { "shfmt" },
  yml = { "prettier" },
  yaml = { "prettier" },
  markdown = { "prettier" },
  javascript = { "prettier" },
  javascriptreact = { "prettier" },
  typescript = { "prettier" },
  typescriptreact = { "prettier" },
  json = { "prettier" },
  css = { "prettier" },
  html = { "prettier" },
}

require("conform").setup({
  formatters_by_ft = formatters_by_ft,
  notify_on_error = true,
  -- format_on_save = format_on_save_opts,
})

vim.api.nvim_create_user_command('PrettierFormat', function()
  require("conform").format(format_on_save_opts)
end, { desc = "Format with Prettier" })

vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = {
    "*.js",
    "*.jsx",
    "*.ts",
    "*.tsx",
    "*.json",
    "*.css",
    "*.html",
    "*.md",
    "*.yaml",
    "*.yml",
  },
  command = "PrettierFormat",
})

vim.keymap.set({ "n", "v" }, "g=", function()
  require("conform").format(format_on_save_opts)
end, { desc = "[G]et [=]format conform" })
