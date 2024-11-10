vim.keymap.set("n", "<leader>gb", ":Git blame<CR>", { desc = "[G]it [B]lame" })

local gitsigns = require("gitsigns")
gitsigns.setup()

vim.keymap.set("n", "<Leader>gB", gitsigns.blame_line, { desc = "Git [B]lame" })

vim.keymap.set("n", "]c", gitsigns.next_hunk, { desc = "Next [C]hange" })
vim.keymap.set("n", "[c", gitsigns.prev_hunk, { desc = "Prev [C]hange" })

vim.keymap.set("n", "<leader>hp", gitsigns.preview_hunk, { desc = "[H]unk [P]review" })
vim.keymap.set("n", "<leader>hP", gitsigns.preview_hunk_inline, { desc = "[H]unk [P]review inline" })
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
vim.keymap.set("n", "<leader>hd", gitsigns.diffthis, { desc = "[H]unk diff" })

-- Text object inside hunk
vim.keymap.set({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", { desc = "[i]nside [H]unk" })
vim.keymap.set({ "o", "x" }, "ah", ":<C-U>Gitsigns select_hunk<CR>", { desc = "[A]rround [H]unk" })
