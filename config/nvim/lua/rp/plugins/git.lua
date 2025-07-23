-- fugitive
vim.keymap.set("n", "<leader>gg", ":Git<CR>", { desc = "[G]it" })
vim.keymap.set("n", "<leader>gb", ":Git blame<CR>", { desc = "[G]it [B]lame" })
vim.keymap.set("n", "<leader>gd", ":Gvdiffsplit<CR>", { desc = "[G]it [D]iff" })
vim.keymap.set("n", "<leader>ge", ":Gvsplit", { desc = "[G]it [E]dit <path>" })
-- when inside a blob, us this to go up the tree
vim.keymap.set("n", "<leader>gu", ":edit %:h", { desc = "[G]it [U]p object tree" })

-- add [q] to quit fugitive and fugitive related windows
vim.api.nvim_create_augroup("myFugitive", { clear = true })
vim.api.nvim_create_autocmd("WinEnter", {
  group = "myFugitive",
  pattern = "*",
  callback = function()
    if not vim.wo.diff then
      return
    end

    local bufnr = vim.api.nvim_get_current_buf()

    vim.keymap.set("n", "q", function()
      for _, win in ipairs(vim.api.nvim_list_wins()) do
        if vim.api.nvim_win_is_valid(win) and vim.wo[win].diff then
          pcall(vim.keymap.del, "n", "q", { buffer = bufnr })
          vim.api.nvim_win_close(win, true)
        end
      end
    end, { buffer = bufnr, silent = true, remap = false, desc = "Close all diff windows" })
  end,
})
vim.api.nvim_create_autocmd("FileType", {
  group = "myFugitive",
  pattern = { "fugitive", "fugitiveblame", "git" },
  callback = function()
    vim.keymap.set("n", "q", ":q<CR>", { buffer = true, remap = false })
  end,
})
-- update fugitive window size
vim.api.nvim_create_autocmd("FileType", {
  pattern = "fugitive",
  callback = function()
    vim.cmd("resize 15")
  end,
})
-- update gitcommit window size
vim.api.nvim_create_autocmd("FileType", {
  pattern = "gitcommit",
  callback = function()
    vim.cmd("wincmd K")
  end
})

-- neogit
local neogit = require("neogit")
vim.keymap.set("n", "<leader>gn", function() neogit.open({ kind = "split" }) end, { desc = "[G]it [N]eogit" })

-- gitsignes
local gitsigns = require("gitsigns")
gitsigns.setup()

vim.keymap.set("n", "<Leader>gB", gitsigns.blame_line, { desc = "Git [B]lame" })

vim.keymap.set("n", "]h", function() gitsigns.nav_hunk('next') end, { desc = "Next [H]unk" })
vim.keymap.set("n", "[h", function() gitsigns.nav_hunk('prev') end, { desc = "Prev [H]unk" })

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
vim.keymap.set("n", "<leader>hu", gitsigns.stage_hunk, { desc = "[H]unk [U]nstage" })
vim.keymap.set("n", "<leader>hS", gitsigns.stage_buffer, { desc = "[H]unks [S]tage in buffer" })
vim.keymap.set("n", "<leader>hR", gitsigns.reset_buffer, { desc = "[H]unks [R]eset in buffer" })
vim.keymap.set("n", "<leader>hd", gitsigns.diffthis, { desc = "[H]unk diff" })

-- Text object inside hunk
vim.keymap.set({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", { desc = "[i]nside [H]unk" })
vim.keymap.set({ "o", "x" }, "ah", ":<C-U>Gitsigns select_hunk<CR>", { desc = "[A]rround [H]unk" })
