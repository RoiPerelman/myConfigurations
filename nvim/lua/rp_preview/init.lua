local M = {}

local ok, rp_messages = pcall(require, "rp_messages")
if not ok then
  M.print = vim.print
else
  M.print = rp_messages.print
end

function M.scroll_preview_up()
  if M.preview_win and vim.api.nvim_win_is_valid(M.preview_win) then
    vim.api.nvim_win_call(M.preview_win, function()
      vim.cmd("normal! \\<C-u>") -- Executes Ctrl-u in normal mode
    end)
  end
end

function M.scroll_preview_down()
  if M.preview_win and vim.api.nvim_win_is_valid(M.preview_win) then
    vim.api.nvim_win_call(M.preview_win, function()
      vim.cmd("normal! \\<C-d>") -- Executes Ctrl-d in normal mode
    end)
  end
end

function M.preview_entry(entry, preview_win)
  if not entry or not preview_win then
    return
  end
  if not vim.api.nvim_win_is_valid(preview_win) then
    return
  end

  local bufnr = entry.bufnr
  if not bufnr then
    bufnr = vim.fn.bufadd(entry.filename)
  end

  if not vim.api.nvim_buf_is_valid(bufnr) then
    return
  end

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
  M.print("ROIROI entry", entry)
  return entry
end

function M.setup_quickfix_preview()
  -- Create an autocommand group with a clear option to avoid duplicates
  local augroup_id = vim.api.nvim_create_augroup("QuickfixAutoCmd", { clear = true })
  local preview_win = nil
  local preview_win_buf = nil
  -- TODO: add buffer deletion when exiting quickfix
  -- local delete_buffer_list = {}

  -- local function set_preview_keymaps()
  --   vim.api.nvim_buf_set_keymap(
  --     preview_win_buf,
  --     "n",
  --     "<C-u>",
  --     "",
  --     { noremap = true, silent = true, callback = M.scroll_preview_up }
  --   )
  --   vim.api.nvim_buf_set_keymap(
  --     preview_win_buf,
  --     "n",
  --     "<C-d>",
  --     "",
  --     { noremap = true, silent = true, callback = M.scroll_preview_down }
  --   )
  -- end
  --
  -- local function remove_preview_keymaps()
  --   vim.api.nvim_buf_del_keymap(preview_win_buf, "n", "<C-u>")
  --   vim.api.nvim_buf_del_keymap(preview_win_buf, "n", "<C-d>")
  -- end

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
          M.print("BufEnter Quickfix preview_win", preview_win)
          preview_win_buf = vim.api.nvim_win_get_buf(preview_win)
          M.print("BufEnter Quickfix preview_win_buf", preview_win_buf)
          M.preview_entry(M.get_qflist_entry(), preview_win)
          -- set_preview_keymaps()
        end
        if preview_win and preview_win_buf ~= vim.api.nvim_win_get_buf(preview_win) then
          -- remove_preview_keymaps()
          preview_win_buf = vim.api.nvim_win_get_buf(preview_win)
          -- set_preview_keymaps()
          M.print("BufEnter Quickfix updated preview_win_buf", preview_win_buf)
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
          M.print("BufLeave Quickfix set og preview_win_buf to preview_win")
        end
      end,
    })

    vim.api.nvim_create_autocmd("BufWinLeave", {
      group = augroup_id,
      buffer = bufnr,
      callback = function()
        M.print("BufWinLeave Quickfix list")
        -- if preview_win and vim.api.nvim_win_is_valid(preview_win) then
        --   remove_preview_keymaps()
        -- end
        preview_win = nil
        preview_win_buf = nil
        M.print("BufWinLeave Quickfix removed preview_win and preview_win_buf")
      end,
    })
  end

  -- start autocommands on qf buffer
  vim.cmd("copen")
  local bufnr = vim.api.nvim_get_current_buf() -- Get the current buffer number
  add_quickfix_autocommands(bufnr)
  vim.cmd("cclose")
end

function M.get_fzf_lua_previewer()
  local MyPreviewer = require("fzf-lua.previewer.builtin").base:extend()
  local path = require("fzf-lua.path")

  MyPreviewer.preview_win = nil
  MyPreviewer.preview_win_buf = nil

  function MyPreviewer:new(o, opts, fzf_win)
    -- hack to remove preview window
    fzf_win.winopts.preview.vertical = "down:0%"
    fzf_win.winopts.preview.horizontal = "right:0%"
    MyPreviewer.super.new(self, {}, opts, fzf_win)
    MyPreviewer.preview_win = vim.fn.win_getid(vim.fn.winnr("#"))
    MyPreviewer.preview_win_buf = vim.api.nvim_win_get_buf(MyPreviewer.preview_win)
    setmetatable(self, MyPreviewer)
    return self
  end

  function MyPreviewer:display_entry(entry_str)
    local file = path.entry_to_file(entry_str, self.opts)
    local text = entry_str:match(":%d+:%d?%d?%d?%d?:?(.*)$")
    local entry = {
      filename = file.bufname or file.path or file.uri,
      lnum = file.line,
      col = file.col,
      text = text,
    }

    M.preview_entry(entry, MyPreviewer.preview_win)
  end

  function MyPreviewer:close()
    vim.api.nvim_win_set_buf(MyPreviewer.preview_win, MyPreviewer.preview_win_buf)
    MyPreviewer.preview_win = nil
    MyPreviewer.preview_win_buf = nil
    MyPreviewer.super.close(self)
  end
  return MyPreviewer
end

return M
