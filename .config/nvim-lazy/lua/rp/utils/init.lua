M = {}

---@type table<number, string>
M.cache = {}

function M.find_git_root()
  -- Get the current buffer number
  local buf = vim.api.nvim_get_current_buf()

  local patterns = { ".git" }
  local path = vim.api.nvim_buf_get_name(buf) or vim.fn.getcwd()
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

return M
