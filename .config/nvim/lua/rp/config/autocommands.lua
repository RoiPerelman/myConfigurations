-- local rp_preview = require("rp_preview")
-- rp_preview.setup_quickfix_preview()

-- TODO go over lazyvim autocommands
-- local function augroup(name)
--   return vim.api.nvim_create_augroup("rp" .. name, { clear = true })
-- end

vim.cmd([[
  augroup last_edit_position
    autocmd!
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
  augroup END
]])

vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking text",
  group = vim.api.nvim_create_augroup("rp-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

vim.cmd([[
  augroup remove_trailing_whitespace
    autocmd!
    au BufWritePre * %s/\s\+$//e
  augroup END
]])

vim.api.nvim_create_autocmd("BufWritePost", {
  callback = function()
    -- Environment variables
    local project_path = os.getenv("INSPEKTO_PROJECT")
    local remote_path = os.getenv("INSPEKTO_REMOTE_PATH") or "rp@rp-il.net.plm.eds.com:/home/rp/sinspekto/winspekto"

    -- Return early if env variable is not set
    if not project_path then
      return
    end

    -- Get the current file's path
    local current_file = vim.fn.expand('%:p')

    -- Check if the file is in the project directory
    if string.match(current_file, project_path) then
      vim.notify(string.format("Rsync started to %s", remote_path), vim.log.levels.INFO)
      local cmd = string.format('rsync -avz --delete --progress --exclude="*/__pycache__" --exclude="*.pyc" %s/ %s/', project_path, remote_path)

      -- Buffer to collect output
      local output_buffer = {}
      local error_buffer = {}

      vim.fn.jobstart(cmd, {
        on_stdout = function(_, data)
          if data and data[1] ~= "" then
            table.insert(output_buffer, table.concat(data, "\n"))
          end
        end,
        on_stderr = function(_, data)
          if data and data[1] ~= "" then
            table.insert(error_buffer, table.concat(data, "\n"))
          end
        end,
        on_exit = function(_, exit_code)
          vim.schedule(function()
            if exit_code == 0 then
              local success_msg = "Rsync completed successfully\n\n"
              if #output_buffer > 0 then
                success_msg = success_msg .. table.concat(output_buffer, "\n"):gsub("\r", "\n")
              end
              vim.notify(success_msg, vim.log.levels.INFO)
            else
              local error_msg = string.format("Rsync failed with exit code: %d\n\n", exit_code)
              if #error_buffer > 0 then
                error_msg = error_msg .. table.concat(error_buffer, "\n"):gsub("\r", "\n")
              end
              vim.notify(error_msg, vim.log.levels.ERROR)
            end
          end)
        end
      })
    end
  end,
})
