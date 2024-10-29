local rp_vertico = require('rp-vertico')
local fzf = require('rp-vertico.fzf')
local Utils = require('rp-vertico.utils')

local M = {}

M.find_files = function(opts)
  local command = function(cache, is_init)
    if is_init then
      local command = { "rg", "--files", "--color", "never", "-g", "!.git", "--hidden" }

      -- add directories to search for command
      if opts and opts.dirs and type(opts.dirs) == "table" then
        for _, dir in ipairs(opts.dirs) do
          table.insert(command, vim.fn.expand(dir))
        end
      end

      vim.notify(table.concat(command, " "))

      Utils.shell_command(command, function(lines)
        cache.items = vim.tbl_map(function(line) return { path = line, text = line } end, lines)
        fzf.fzf_filter_sort(cache)
      end)
    else
      vim.schedule(function()
        fzf.fzf_filter_sort(cache)
      end)
    end
  end
  local close_cb = function()
    -- clear fzf cache
    fzf.cache = {}
    fzf.matches = {}
  end
  rp_vertico.open({ command = command, close_cb = close_cb })
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
          local lua_table = vim.json.decode(item)

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
