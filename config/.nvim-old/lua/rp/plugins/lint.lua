local M = {}

local lint = require("lint")
lint.linters_by_ft = {
  sh = { "shellcheck" },
  markdown = { "markdownlint-cli2" },
  ["markdown.mdx"] = { "markdownlint-cli2" },
}
if not lint.linters_by_ft then
  vim.print("nvim-lint doesnt have linters. Not working")
  return
end

function M.debounce(ms, fn)
  local timer = vim.uv.new_timer()
  return function(...)
    local argv = { ... }
    timer:start(ms, 0, function()
      timer:stop()
      vim.schedule_wrap(fn)(unpack(argv))
    end)
  end
end

function M.lint()
  -- Use nvim-lint's logic first:
  -- * checks if linters exist for the full filetype first
  -- * otherwise will split filetype by "." and add all those linters
  -- * this differs from conform.nvim which only uses the first filetype that has a formatter
  local names = lint._resolve_linter_by_ft(vim.bo.filetype)

  -- Create a copy of the names table to avoid modifying the original.
  names = vim.list_extend({}, names)

  -- Add fallback linters.
  if #names == 0 then
    vim.list_extend(names, lint.linters_by_ft["_"] or {})
  end

  -- Add global linters.
  vim.list_extend(names, lint.linters_by_ft["*"] or {})

  -- Filter out linters that don't exist or don't match the condition.
  local ctx = { filename = vim.api.nvim_buf_get_name(0) }
  ctx.dirname = vim.fn.fnamemodify(ctx.filename, ":h")
  names = vim.tbl_filter(function(name)
    local linter = lint.linters[name]
    if not linter then
      vim.print("Linter not found: " .. name, { title = "nvim-lint" })
    end
    return not not linter
  end, names)

  -- Run linters.
  if #names > 0 then
    lint.try_lint(names)
  end
end

vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost", "InsertLeave", "cursorHold" }, {
  group = vim.api.nvim_create_augroup("nvim-lint", { clear = true }),
  callback = M.debounce(100, M.lint),
})
