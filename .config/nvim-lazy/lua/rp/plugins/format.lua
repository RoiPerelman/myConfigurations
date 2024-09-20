return {
  "stevearc/conform.nvim",
  config = function(_, _opts)
    local format_on_save_opts = {
      -- lsp_fallback = true,
      async = false,
      timeout_ms = 1000,
    }

    local formatters = vim.tbl_deep_extend("force", _opts.formatters, {})
    local formatters_by_ft = vim.tbl_deep_extend("force", _opts.formatters_by_ft, {
      lua = { "stylua" },
    })

    local opts = vim.tbl_deep_extend("force", _opts, {
      formatters = formatters,
      formatters_by_ft = formatters_by_ft,
      notify_on_error = true,
      -- format_on_save = format_on_save_opts,
    })

    require("conform").setup(opts)
    vim.keymap.set({ "n", "v" }, "g=", function()
      require("conform").format(format_on_save_opts)
    end, { desc = "Format file or range (in visual mode)" })
  end,
}
