local rp_vertico = require('rp-vertico')
local M = {}

M.find_files = function()
  local initial_command = function(cache)
    local shell_command = { "rg", "--files", "--color", "never", "-g", "!.git", "--hidden" }
    local executable, args = shell_command[1], vim.list_slice(shell_command, 2, #shell_command)

    local process, pid, stdout = nil, nil, vim.loop.new_pipe()

    local stdout = vim.loop.new_pipe()
    local options = {
      args = args,
      stdio = { nil, stdout, nil }
    }
    process, pid = vim.loop.spawn(executable, options, function()
      if process and process:is_active() then process:close() end
    end)

    local data_feed = {}
    stdout:read_start(function(err, data)
      assert(not err, err)

      -- fill data_feed with data
      if data ~= nil then return table.insert(data_feed, data) end

      -- create items from full data_feed
      local items = vim.split(table.concat(data_feed), '\n')
      data_feed = nil
      -- and close the pipe
      stdout:close()
      cache.items = items
    end)
  end
  rp_vertico.open({ initial_command = initial_command })
end

return M
