--- Navigation

-- make navigation work with wrapped lines
vim.keymap.set({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { desc = "Down", expr = true, silent = true })
vim.keymap.set({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { desc = "Up", expr = true, silent = true })
vim.keymap.set({ "n", "x" }, "$", "v:count == 0 ? 'g$' : '$'", { desc = "Down", expr = true, silent = true })
vim.keymap.set({ "n", "x" }, "^", "v:count == 0 ? 'g^' : '^'", { desc = "Up", expr = true, silent = true })
vim.keymap.set({ "n", "x" }, "0", "v:count == 0 ? 'g0' : '0'", { desc = "Up", expr = true, silent = true })

-- indenting (do not lose visual mode)
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

--- Windows

-- better window navigation See `:help wincmd`
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "focus left window" })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "focus right window" })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "focus lower window" })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "focus upper window" })

-- resize windows
vim.keymap.set("n", "<C-Up>", ":resize +2<cr>", { desc = "inc window Height" })
vim.keymap.set("n", "<C-Down>", ":resize -2<cr>", { desc = "dec window Height" })
vim.keymap.set("n", "<C-Left>", ":vertical resize -2<cr>", { desc = "dec window width" })
vim.keymap.set("n", "<C-Right>", ":vertical resize +2<cr>", { desc = "inc window width" })

--- Explor
vim.keymap.set("n", "<leader>e", ":Explor<cr>", { desc = "[E]xplor" })

--- Nice to have
vim.keymap.set("n", "<leader>R", ":update<cr> :source<cr>", { desc = "[E]xplor" })

--- Toggles

-- Toggle whitespace trimming with leader+tw
vim.keymap.set("n", "<leader>tW", function()
  vim.b.rp_disable_trim_whitespace = not vim.b.rp_disable_trim_whitespace
  vim.notify("rp_trim_whitespace " .. (vim.b.rp_disable_trim_whitespace and "disabled" or "enabled"))
end, { desc = "[T]oggle trim [W]hitespace" })

vim.keymap.set("n", "<leader>tw", function()
  local new_value = not vim.opt.wrap:get()
  vim.opt.wrap = new_value
  vim.notify("wrap " .. (new_value and "enabled" or "disabled"))
end, { desc = "[T]oggle [W]rap" })

