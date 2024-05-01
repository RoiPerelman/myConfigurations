local M = {}

M.print = require("rp_messages").print

-- local ok, rp_print = pcall(require("rp_messages").print)
-- M.print = rp_print
-- if not ok then
--   M.print = vim.print
-- end

local preview_buffers = {}

function M.preview_entry(entry, preview_win)
  if not entry or not preview_win then
    return
  end
  if not vim.api.nvim_win_is_valid(preview_win) then
    return
  end
  if not vim.api.nvim_buf_is_valid(entry.bufnr) then
    return
  end

  local bufnr = entry.bufnr

  vim.api.nvim_buf_call(bufnr, function()
    -- Center preview line on screen
    if not vim.api.nvim_buf_is_loaded(bufnr) then
      vim.fn.bufload(bufnr)
    end
    vim.cmd("norm! zz zv")
  end)

  local filetype = vim.filetype.match({ filename = vim.api.nvim_buf_get_name(bufnr) })
  if filetype then
    local lang = vim.treesitter.language.get_lang(filetype)
    if not pcall(vim.treesitter.start, bufnr, lang) then
      vim.bo[bufnr].syntax = filetype
    end
  end

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

  -- window configurations
  vim.api.nvim_win_set_buf(preview_win, bufnr)
  local pos = { entry.lnum, entry.col }
  local line_count = vim.api.nvim_buf_line_count(bufnr)
  pos[1] = math.min(pos[1], line_count)
  vim.api.nvim_win_set_cursor(preview_win, pos)
end

function M.get_qflist_entry()
  local line = vim.fn.line(".")
  local qf_list = vim.fn.getqflist()
  local entry = qf_list[line]
  return entry
end

function M.setup_quickfix_preview()
  -- Create an autocommand group with a clear option to avoid duplicates
  local augroup_id = vim.api.nvim_create_augroup("QuickfixAutoCmd", { clear = true })
  local preview_win = nil
  local preview_win_buf = nil
  local delete_buffer_list = {}

  -- Function to add Quickfix-specific autocommands
  local function add_quickfix_autocommands(bufnr)
    vim.api.nvim_create_autocmd("BufEnter", {
      group = augroup_id,
      buffer = bufnr,
      callback = function()
        M.print("BufEnter Quickfix list")
        if preview_win and not vim.api.nvim_win_is_valid(preview_win) then
          preview_win = nil
        end
        if preview_win_buf and not vim.api.nvim_buf_is_valid(preview_win_buf) then
          preview_win_buf = nil
        end
        if not preview_win and not preview_win_buf then
          preview_win = vim.fn.win_getid(vim.fn.winnr("#"))
          M.print("preview_win", preview_win)
          preview_win_buf = vim.api.nvim_win_get_buf(preview_win)
          M.print("preview_win_buf", preview_win_buf)
          M.preview_entry(M.get_qflist_entry(), preview_win)
        end
        if preview_win and preview_win_buf ~= vim.api.nvim_win_get_buf(preview_win) then
          preview_win_buf = vim.api.nvim_win_get_buf(preview_win)
        end
      end,
    })

    vim.api.nvim_create_autocmd("CursorMoved", {
      group = augroup_id,
      buffer = bufnr,
      callback = function()
        M.print("CursorMoved Quickfix list")
        if preview_win and vim.api.nvim_win_is_valid(preview_win) then
          M.preview_entry(M.get_qflist_entry(), preview_win)
        end
      end,
    })

    vim.api.nvim_create_autocmd("BufLeave", {
      group = augroup_id,
      buffer = bufnr,
      callback = function()
        M.print("BufLeave Quickfix list")
        if preview_win and preview_win_buf then
          vim.api.nvim_win_set_buf(preview_win, preview_win_buf)
        end
      end,
    })

    vim.api.nvim_create_autocmd("BufWinLeave", {
      group = augroup_id,
      buffer = bufnr,
      callback = function()
        M.print("BufWinLeave Quickfix list")
        preview_win = nil
        preview_win_buf = nil
      end,
    })
  end

  -- start autocommands on qf buffer
  vim.cmd("copen")
  local bufnr = vim.api.nvim_get_current_buf() -- Get the current buffer number
  add_quickfix_autocommands(bufnr)
  vim.cmd("cclose")
end

return M
