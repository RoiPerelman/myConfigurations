local format_on_save_opts = {
  -- lsp_fallback = true,
  async = false,
  timeout_ms = 1000,
}

local formatters_by_ft = {
  sh = { "shfmt" },
  yml = { "prettier" },
  yaml = { "prettier" },
}

require("conform").setup({
  formatters_by_ft = formatters_by_ft,
  notify_on_error = true,
  -- format_on_save = format_on_save_opts,
})

vim.keymap.set({ "n", "v" }, "g=", function()
  require("conform").format(format_on_save_opts)
end, { desc = "[G]et [=]format conform" })
