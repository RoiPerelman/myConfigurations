return { -- Autoformat
  "stevearc/conform.nvim",
  config = function()
    local format_on_save_opts = {
      lsp_fallback = true,
      async = false,
      timeout_ms = 1000,
    }
    require("conform").setup({

      formatters_by_ft = {
        lua = { "stylua" },
        -- python = { "isort", "black" },
      },
      notify_on_error = true,
      format_on_save = format_on_save_opts,
    })
    vim.keymap.set({ "n", "v" }, "g=", function()
      require("conform").format(format_on_save_opts)
    end, { desc = "Format file or range (in visual mode)" })
  end,
}
