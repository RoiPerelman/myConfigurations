local Utils = require('rp-vertico.utils')

local M = {}

M.current_preview = nil

M.close_preview = function()
  M.current_preview = nil
  for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(bufnr) then
      local is_rp_preview = pcall(vim.api.nvim_buf_get_var, bufnr, "is_rp_preview")
      if is_rp_preview then
        vim.api.nvim_buf_delete(bufnr, { force = true })
      end
    end
  end
end

function M.preview_item(preview_win, item)
  if M.current_preview == item.path then
    return
  end

  vim.notify('ROIROI BBB')
  if not (preview_win and vim.api.nvim_win_is_valid(preview_win)) or not item then
    vim.notify('rp-vertico preview_item bad params')
    return
  end

  vim.notify('ROIROI CCC')
  if item.path then
    local norm_path = Utils.norm_path(item.path)
    local buffer_name = 'rp_preview:' .. norm_path
    local buffer = vim.fn.bufnr(buffer_name, false)
    vim.notify('ROIROI CCD' .. tostring(buffer))
    if buffer == -1 then
      buffer = vim.api.nvim_create_buf(false, true) -- Create a scratch buffer
      vim.notify('ROIROI DDD' .. tostring(buffer))
      vim.notify('ROIROI DEE' .. tostring(norm_path))

      vim.api.nvim_buf_set_var(buffer, "is_rp_preview", true)
      vim.api.nvim_buf_set_name(buffer, buffer_name)

      vim.notify('ROIROI EEE')
      -- Read file contents
      local file = io.open(norm_path, "r")
      if file then
        local content = {}
        for line in file:lines() do
          table.insert(content, line)
        end
        file:close()
        vim.api.nvim_buf_set_lines(buffer, 0, -1, false, content)
      else
        vim.notify("rp-vertico preview Failed to open file: " .. norm_path)
        return
      end
      vim.notify('ROIROI FFF')

      local filetype = vim.filetype.match({ filename = norm_path })
      if filetype then
        vim.bo[buffer].filetype = filetype
      end
    end

    vim.notify('ROIROI GGG')
    vim.api.nvim_buf_call(buffer, function()
      -- center (zz) and open folds on line (zv)
      vim.cmd("norm! zz zv")
    end)

    -- Disable LSP
    -- for _, client in pairs(vim.lsp.get_clients()) do
    --   vim.lsp.buf_detach_client(buffer, client.id)
    -- end

    vim.notify('ROIROI HHH')
    vim.api.nvim_win_set_buf(preview_win, buffer)
    vim.notify('ROIROI III')
    -- local pos = { entry.lnum, entry.col }
    -- local line_count = vim.api.nvim_buf_line_count(bufnr)
    -- pos[1] = math.min(pos[1], line_count)
    -- vim.api.nvim_win_set_cursor(preview_win, pos)
  end

  M.current_preview = item.path


  -- -- make sure we highlight at least one character
  -- local end_pos = { item.end_pos[1], item.end_pos[2] }
  -- if end_pos[1] == item.pos[1] and end_pos[2] == item.pos[2] then
  --   end_pos[2] = end_pos[2] + 1
  -- end
  --
  -- -- highlight the line
  -- vim.api.nvim_buf_set_extmark(buf, Render.ns, item.pos[1] - 1, 0, {
  --   end_row = end_pos[1],
  --   hl_group = "CursorLine",
  --   hl_eol = true,
  --   strict = false,
  -- })
  --
  -- -- highlight the range
  -- vim.api.nvim_buf_set_extmark(buf, Render.ns, item.pos[1] - 1, item.pos[2], {
  --   end_row = end_pos[1] - 1,
  --   end_col = end_pos[2],
  --   hl_group = "TroublePreview",
  --   strict = false,
  -- })
  --
  -- -- Disable LSP for this buffer
  -- vim.lsp.buf_detach_client(bufnr)
end

return M
