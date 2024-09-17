M = {}

---@type table<number, string>
M.cache = {}

function M.find_git_root()
  local patterns = { ".git" }

  -- get the full path using current buffer name
  local buf = vim.api.nvim_get_current_buf()
  local buf_name = vim.api.nvim_buf_get_name(buf)
  if buf_name == '' then
    -- Get the full path using vim.fn.expand('%:p') - can catch more cases
    buf_name = vim.fn.expand('%:p')
  end
  local path = (buf_name ~= '' and buf_name) or vim.fn.getcwd()
  local pattern = vim.fs.find(function(name)
    for _, p in ipairs(patterns) do
      if name == p then
        return true
      end
      if p:sub(1, 1) == "*" and name:find(vim.pesc(p:sub(2)) .. "$") then
        return true
      end
    end
    return false
  end, { path = path, upward = true })[1]
  return vim.fs.dirname(pattern)
end

function M.find_buf_root()
  -- Get the current buffer number
  local buf = vim.api.nvim_get_current_buf()

  if not M.cache[buf] then
    local root = M.find_git_root()
    M.cache[buf] = root
  end

  return M.cache[buf]
end

return M.find_buf_root
