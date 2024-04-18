function view_messages_in_scratch()
  -- Create a new buffer and switch to it
  vim.cmd("enew")
  -- Set the buffer-specific options to make it a scratch buffer
  vim.bo.buftype = "nofile"
  vim.bo.bufhidden = "hide"
  vim.bo.swapfile = false

  -- Get the messages and put them into the buffer
  local messages = vim.api.nvim_exec("messages", true)
  local lines = vim.split(messages, "\n", true) -- Split the messages into lines
  vim.api.nvim_put(lines, "", false, true)
end

-- Add a command to Neovim to call this function easily
vim.api.nvim_create_user_command("Msg", view_messages_in_scratch, {})
