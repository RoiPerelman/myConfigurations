return {
  -- See `:help gitsigns` to understand what the configuration keys do
  -- Adds git related signs to the gutter, as well as utilities for managing changes
  "lewis6991/gitsigns.nvim",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    local gitsigns = require("gitsigns")
    gitsigns.setup()

    vim.keymap.set("n", "<Leader>gb", gitsigns.toggle_current_line_blame, { desc = "[G]it [B]lame Line Toggle" })
    vim.keymap.set("n", "<Leader>gB", gitsigns.blame_line, { desc = "[G]it [B]lame" })

    vim.keymap.set("n", "]h", gitsigns.next_hunk, { desc = "Next Hunk" })
    vim.keymap.set("n", "[h", gitsigns.prev_hunk, { desc = "Prev Hunk" })

    vim.keymap.set("n", "<leader>hp", gitsigns.preview_hunk, { desc = "[H]unk [P]review" })
    vim.keymap.set("n", "<leader>hs", gitsigns.stage_hunk, { desc = "[H]unk [S]tage" })
    vim.keymap.set("v", "<leader>hs", function()
      gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
    end, { desc = "[H]unk [S]tage" })
    vim.keymap.set("n", "<leader>hr", gitsigns.reset_hunk, { desc = "[H]unk [R]eset" })
    vim.keymap.set("v", "<leader>hr", function()
      gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
    end, { desc = "[H]unk [R]eset" })
    vim.keymap.set("n", "<leader>hu", gitsigns.undo_stage_hunk, { desc = "[H]unk [U]nstage" })
    vim.keymap.set("n", "<leader>hS", gitsigns.stage_buffer, { desc = "[H]unks [S]tage in buffer" })
    vim.keymap.set("n", "<leader>hR", gitsigns.reset_buffer, { desc = "[H]unks [R]eset in buffer" })

    -- Text object inside hunk
    vim.keymap.set({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", { desc = "[i]nside [H]unk" })
    vim.keymap.set({ "o", "x" }, "ah", ":<C-U>Gitsigns select_hunk<CR>", { desc = "[A]rround [H]unk" })
  end,
}
