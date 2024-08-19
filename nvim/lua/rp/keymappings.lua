vim.keymap.set({ "i", "n" }, "<esc>", "<cmd>noh<cr><esc>", { desc = "Escape and Clear hlsearch" })

-- arrows are for pussies (can use <NOP>)
vim.keymap.set("n", "<left>", '<cmd>echo "Use h to move!!"<CR>')
vim.keymap.set("n", "<right>", '<cmd>echo "Use l to move!!"<CR>')
vim.keymap.set("n", "<up>", '<cmd>echo "Use k to move!!"<CR>')
vim.keymap.set("n", "<down>", '<cmd>echo "Use j to move!!"<CR>')

-- TODO: understand this line
-- better up/down
vim.keymap.set({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { desc = "Down", expr = true, silent = true })
vim.keymap.set({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { desc = "Up", expr = true, silent = true })

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

-- better window navigation See `:help wincmd`
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Move focus to the left window" }) -- :wincmd h<CR>
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Move focus to the right window" }) -- :wincmd l<CR>
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Move focus to the lower window" }) -- :wincmd j<CR>
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Move focus to the upper window" }) -- :wincmd k<CR>

-- resize windows
vim.keymap.set("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase Window Height" })
vim.keymap.set("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease Window Height" })
vim.keymap.set("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease Window Width" })
vim.keymap.set("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase Window Width" })

-- move lines up and down
vim.keymap.set("n", "<A-j>", "<cmd>m .+1<cr>==", { desc = "Move Down" })
vim.keymap.set("n", "<A-k>", "<cmd>m .-2<cr>==", { desc = "Move Up" })
vim.keymap.set("i", "<A-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move Down" })
vim.keymap.set("i", "<A-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move Up" })
vim.keymap.set("v", "<A-j>", ":m '>+1<cr>gv=gv", { desc = "Move Down" })
vim.keymap.set("v", "<A-k>", ":m '<-2<cr>gv=gv", { desc = "Move Up" })

-- https://github.com/mhinz/vim-galore#saner-behavior-of-n-and-n
vim.keymap.set("n", "n", "'Nn'[v:searchforward].'zv'", { expr = true, desc = "Next Search Result" })
vim.keymap.set("x", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
vim.keymap.set("o", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
vim.keymap.set("n", "N", "'nN'[v:searchforward].'zv'", { expr = true, desc = "Prev Search Result" })
vim.keymap.set("x", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })
vim.keymap.set("o", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })

-- new empty buffer
vim.keymap.set("n", "<leader>fn", "<cmd>enew<cr>", { desc = "New File" })

-- cut does so to black hole register!!!
vim.keymap.set("n", "c", '"_c')
vim.keymap.set("n", "C", '"_C')
vim.keymap.set("v", "c", '"_c')
vim.keymap.set("v", "C", '"_C')
vim.keymap.set("x", "p", [["_dP"]])

-- -- highlights under cursor
-- vim.keymap.set("n", "<leader>ui", vim.show_pos, { desc = "Inspect Pos" })
-- vim.keymap.set("n", "<leader>uI", "<cmd>InspectTree<cr>", { desc = "Inspect Tree" })

-- next prev key bindings
-- buffers
vim.keymap.set("n", "]b", ":bnext<CR>")
vim.keymap.set("n", "[b", ":bprevious<CR>")
-- quickfix and locallist
vim.keymap.set("n", "]q", ":cnext<CR>")
vim.keymap.set("n", "[q", ":cprev<CR>")
vim.keymap.set("n", "]l", ":lnext<CR>")
vim.keymap.set("n", "[l", ":lprev<CR>")
-- diagnostic
local diagnostic_goto = function(next, severity)
  local go = next and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
  severity = severity and vim.diagnostic.severity[severity] or nil
  return function()
    go({ severity = severity })
  end
end
vim.keymap.set("n", "<leader>cd", vim.diagnostic.open_float, { desc = "Line Diagnostics" })
vim.keymap.set("n", "]d", diagnostic_goto(true), { desc = "Next Diagnostic" })
vim.keymap.set("n", "[d", diagnostic_goto(false), { desc = "Prev Diagnostic" })
vim.keymap.set("n", "]e", diagnostic_goto(true, "ERROR"), { desc = "Next Error" })
vim.keymap.set("n", "[e", diagnostic_goto(false, "ERROR"), { desc = "Prev Error" })
vim.keymap.set("n", "]w", diagnostic_goto(true, "WARN"), { desc = "Next Warning" })
vim.keymap.set("n", "[w", diagnostic_goto(false, "WARN"), { desc = "Prev Warning" })

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
vim.api.nvim_set_keymap(
  "n",
  "<leader>q",
  ":ToggleQuickfix<CR>",
  { noremap = true, silent = true, desc = "[Q]uickfix Toggle" }
)

-- Map <leader>l to toggle the location list window
vim.api.nvim_set_keymap(
  "n",
  "<leader>l",
  ":ToggleLoclist<CR>",
  { noremap = true, silent = true, desc = "[L]ocalist Toggle" }
)
