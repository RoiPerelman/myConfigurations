local ts_repeat_move = require "nvim-treesitter.textobjects.repeatable_move"

-- Repeat movement with ; and ,
-- ensure ; goes forward and , goes backward regardless of the last direction
vim.keymap.set({ "n", "x", "o" }, ";", ts_repeat_move.repeat_last_move_next)
vim.keymap.set({ "n", "x", "o" }, ",", ts_repeat_move.repeat_last_move_previous)

-- Add for f, F, t, T
vim.keymap.set({ "n", "x", "o" }, "f", ts_repeat_move.builtin_f_expr, { expr = true })
vim.keymap.set({ "n", "x", "o" }, "F", ts_repeat_move.builtin_F_expr, { expr = true })
vim.keymap.set({ "n", "x", "o" }, "t", ts_repeat_move.builtin_t_expr, { expr = true })
vim.keymap.set({ "n", "x", "o" }, "T", ts_repeat_move.builtin_T_expr, { expr = true })

-- Repeatable unimpaired
local next_qf_repeat, prev_qf_repeat = ts_repeat_move.make_repeatable_move_pair(vim.cmd.cnext, vim.cmd.cprev)
vim.keymap.set("n", "]q", next_qf_repeat, { desc = "Next Quickfix" })
vim.keymap.set("n", "[q", prev_qf_repeat, { desc = "Prev Quickfix" })

local next_loc, prev_loc = ts_repeat_move.make_repeatable_move_pair(vim.cmd.lnext, vim.cmd.lprev)
vim.keymap.set("n", "]l", next_loc, { desc = "Next location" })
vim.keymap.set("n", "[l", prev_loc, { desc = "Prev location" })

local next_tag, prev_tag = ts_repeat_move.make_repeatable_move_pair(vim.cmd.tnext, vim.cmd.tprev)
vim.keymap.set("n", "]t", next_tag, { desc = "Next tag" })
vim.keymap.set("n", "[t", prev_tag, { desc = "Prev tag" })

local next_arg, prev_arg = ts_repeat_move.make_repeatable_move_pair(vim.cmd.argnext, vim.cmd.argprev)
vim.keymap.set("n", "]a", next_arg, { desc = "Next argument" })
vim.keymap.set("n", "[a", prev_arg, { desc = "Prev argument" })

local next_buf, prev_buf = ts_repeat_move.make_repeatable_move_pair(vim.cmd.bnext, vim.cmd.bprev)
vim.keymap.set("n", "]b", next_buf, { desc = "Next buffer" })
vim.keymap.set("n", "[b", prev_buf, { desc = "Prev buffer" })

-- Repeatable diagnostic navigation
local diagnostic_goto = function(next, severity)
  severity = severity and vim.diagnostic.severity[severity] or nil
  return function()
    vim.diagnostic.jump({
      count = next and 1 or -1,
      severity = severity,
      float = true,
    })
  end
end

local next_diag, prev_diag = ts_repeat_move.make_repeatable_move_pair(
  diagnostic_goto(true),
  diagnostic_goto(false)
)
vim.keymap.set("n", "]d", next_diag, { desc = "Next Diagnostic" })
vim.keymap.set("n", "[d", prev_diag, { desc = "Prev Diagnostic" })

local next_error, prev_error = ts_repeat_move.make_repeatable_move_pair(
  diagnostic_goto(true, "ERROR"),
  diagnostic_goto(false, "ERROR")
)
vim.keymap.set("n", "]e", next_error, { desc = "Next Error" })
vim.keymap.set("n", "[e", prev_error, { desc = "Prev Error" })

local next_warn, prev_warn = ts_repeat_move.make_repeatable_move_pair(
  diagnostic_goto(true, "WARN"),
  diagnostic_goto(false, "WARN")
)
vim.keymap.set("n", "]w", next_warn, { desc = "Next Warning" })
vim.keymap.set("n", "[w", prev_warn, { desc = "Prev Warning" })

-- Repeatable hunk navigation
local ok, gitsigns = pcall(require, 'gitsigns')
if not ok then
  vim.notify('gitsigns not found!', vim.log.levels.WARN)
else
  local next_hunk_repeat, prev_hunk_repeat = ts_repeat_move.make_repeatable_move_pair(
    function() gitsigns.nav_hunk('next') end,
    function() gitsigns.nav_hunk('prev') end
  )

  vim.keymap.set({ "n", "x", "o" }, "]h", next_hunk_repeat, { desc = "Next hunk" })
  vim.keymap.set({ "n", "x", "o" }, "[h", prev_hunk_repeat, { desc = "Prev hunk" })
end
