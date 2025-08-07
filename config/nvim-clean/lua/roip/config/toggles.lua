local function toggle_option(option)
  return function()
    local current_value = vim.opt[option]:get()
    local new_value = not current_value
    vim.opt[option] = new_value
    vim.notify(option .. " " .. (new_value and "enabled" or "disabled"))
  end
end

-- toggle wrap
vim.keymap.set("n", "<leader>tw", toggle_option('wrap'), { desc = "[T]oggle [W]rap" })

-- toggle spell (clean file with :runtime spell/cleanadd.vim)
vim.keymap.set("n", "<leader>tz", toggle_option('spell'), { desc = "[T]oggle [S]pell" })

-- toggle invisible
vim.keymap.set("n", "<leader>ti", toggle_option('list'), { desc = "[T]oggle [I]nvisible chars" })

-- toggle whitespace trimming autocommand
vim.keymap.set("n", "<leader>tW", function()
  vim.b.rp_disable_trim_whitespace = not vim.b.rp_disable_trim_whitespace
  vim.notify("rp_trim_whitespace " .. (vim.b.rp_disable_trim_whitespace and "disabled" or "enabled"))
end, { desc = "[T]oggle trim [W]hitespace" })

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
