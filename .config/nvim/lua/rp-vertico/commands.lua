local rp_vertico = require('rp-vertico')
local fzf = require('rp-vertico.fzf')

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
  local fuzzy_command = function(cache)
    fzf.fzf_filter_sort(cache)
    -- vim.schedule(function()
    -- end)
  end
  rp_vertico.open({ initial_command = initial_command, fuzzy_command = fuzzy_command })
end

M.search_grep = function()
  local command = function(cache)
    local pattern = table.concat(cache.query, '')
    -- Shell command with the pattern to search for
    --
    -- local shell_command = { 'rg', '--column', '--line-number', '--no-heading', '--field-match-separator', '\\x00',
    --   '--no-follow', '--color=never', '--', pattern }
    local shell_command = { 'rg', '--json', '--', "ROIROI" }
    -- local shell_command = { "rg", "--vimgrep", "--color", "never", "-g", "!.git", "--hidden", pattern }
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

      -- Fill data_feed with data
      if data ~= nil then
        return table.insert(data_feed, data)
      end

      -- Process the complete data
      local items = vim.split(table.concat(data_feed), '\n')
      local processed_items = {}
      for _, item in pairs(items) do
        if item ~= '' then
          vim.notify('ROIROI ROIROI ROIROI')
          local lua_table = vim.json.decode(item)
          vim.notify('ROIROI 222')

          -- Only process items where type is "match"
          if lua_table.type == "match" then
            local match_entry = {
              path = lua_table.data.path.text,
              line_number = lua_table.data.line_number,
              matches = {}
            }

            -- Collect each submatch start and end position
            for _, submatch in pairs(lua_table.data.submatches) do
              table.insert(match_entry.matches, {
                col_start = submatch.start,
                col_end = submatch["end"]
              })
            end

            -- Add the structured match entry to results
            table.insert(processed_items, match_entry)
          end
        end
      end

      vim.notify('ROIROI')
      vim.notify(vim.inspect(processed_items))
      -- `results` now holds the list of structured objects you described

      data_feed = nil
      -- Close the pipe
      stdout:close()
      cache.items = items
    end)
  end

  -- Open the search interface
  rp_vertico.open({ command = command })
end

return M
