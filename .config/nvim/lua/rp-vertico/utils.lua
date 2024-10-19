local M = {}

M.set_extmark = function(...) pcall(vim.api.nvim_buf_set_extmark, ...) end

return M
