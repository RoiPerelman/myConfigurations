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

  if not (preview_win and vim.api.nvim_win_is_valid(preview_win)) or not item then
    vim.notify('rp-vertico preview_item bad params')
    return
  end

  if item.path then
    local norm_path = Utils.norm_path(item.path)
    local buffer = vim.fn.bufadd(norm_path)
    vim.api.nvim_buf_set_var(buffer, "is_rp_preview", true)
    if not vim.api.nvim_buf_is_valid(buffer) then
      vim.notify('rp-vertico preview_item bufadd failed to give valid buffer')
      return
    end

    -- load buffer
    vim.api.nvim_buf_call(buffer, function()
      -- Center preview line on screen
      if not vim.api.nvim_buf_is_loaded(buffer) then
        vim.fn.bufload(buffer)
      end
      vim.cmd("norm! zz zv")
    end)

    -- local filetype = vim.filetype.match({ filename = vim.api.nvim_buf_get_name(buffer) })
    -- if filetype then
    --   local lang = vim.treesitter.language.get_lang(filetype)
    --   if not pcall(vim.treesitter.start, buffer, lang) then
    --     vim.bo[buffer].syntax = filetype
    --   end
    -- end

    -- window configurations
    vim.api.nvim_win_set_buf(preview_win, buffer)
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
