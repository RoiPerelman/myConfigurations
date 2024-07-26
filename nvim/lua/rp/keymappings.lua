vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- arrows are for pussies (can use <NOP>)
vim.keymap.set("n", "<left>", '<cmd>echo "Use h to move!!"<CR>')
vim.keymap.set("n", "<right>", '<cmd>echo "Use l to move!!"<CR>')
vim.keymap.set("n", "<up>", '<cmd>echo "Use k to move!!"<CR>')
vim.keymap.set("n", "<down>", '<cmd>echo "Use j to move!!"<CR>')

-- i hate escape
vim.keymap.set("i", "jj", "<ESC>")
vim.keymap.set("i", "kk", "<ESC>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")

-- alternate way to save
vim.keymap.set("n", "<C-s>", ":w<CR>")

-- do not loose visual mode when indenting
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

-- move lines up and down
vim.keymap.map("n", "<A-j>", "<cmd>m .+1<cr>==", { desc = "Move Down" })
vim.keymap.map("n", "<A-k>", "<cmd>m .-2<cr>==", { desc = "Move Up" })
vim.keymap.map("i", "<A-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move Down" })
vim.keymap.map("i", "<A-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move Up" })
vim.keymap.map("v", "<A-j>", ":m '>+1<cr>gv=gv", { desc = "Move Down" })
vim.keymap.map("v", "<A-k>", ":m '<-2<cr>gv=gv", { desc = "Move Up" })

-- better window navigation See `:help wincmd`
vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" }) -- :wincmd h<CR>
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" }) -- :wincmd l<CR>
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" }) -- :wincmd j<CR>
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" }) -- :wincmd k<CR>

-- resize windows
vim.keymap.map("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase Window Height" })
vim.keymap.map("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease Window Height" })
vim.keymap.map("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease Window Width" })
vim.keymap.map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase Window Width" })

-- cut does so to black hole register!!!
vim.keymap.set("n", "c", '"_c')
vim.keymap.set("n", "C", '"_C')
vim.keymap.set("v", "c", '"_c')
vim.keymap.set("v", "C", '"_C')
vim.keymap.set("x", "p", [["_dP"]])

-- next prev key bindings
-- buffers
vim.keymap.set("n", "]b", ":bnext<CR>")
vim.keymap.set("n", "[b", ":bprevious<CR>")
-- quickfix and locallist
vim.keymap.set("n", "]q", ":cnext<CR>")
vim.keymap.set("n", "[q", ":cprev<CR>")
vim.keymap.set("n", "]l", ":lnext<CR>")
vim.keymap.set("n", "[l", ":lprev<CR>")

-- Function to toggle the specified window (quickfix or location list)
local function toggle_window(window_type)
  local window_exists = false
  for _, win in ipairs(vim.fn.getwininfo()) do
    if (window_type == "quickfix" and win.quickfix == 1) or (window_type == "loclist" and win.loclist == 1) then
      window_exists = true
      break
    end
  end
  if window_exists then
    vim.cmd(window_type == "quickfix" and "cclose" or "lclose")
  else
    if window_type == "quickfix" then
      vim.cmd("copen")
    elseif window_type == "loclist" then
      local success, _ = pcall(vim.cmd, "lopen")
      if not success then
        print("No location list")
      end
    end
  end
end

-- Create user commands to toggle the quickfix and location list windows
vim.api.nvim_create_user_command("ToggleQuickfix", function()
  toggle_window("quickfix")
end, {})
vim.api.nvim_create_user_command("ToggleLoclist", function()
  toggle_window("loclist")
end, {})

-- Map <leader>q to toggle the quickfix window
vim.api.nvim_set_keymap("n", "<leader>q", ":ToggleQuickfix<CR>", { noremap = true, silent = true })

-- Map <leader>l to toggle the location list window
vim.api.nvim_set_keymap("n", "<leader>l", ":ToggleLoclist<CR>", { noremap = true, silent = true })
