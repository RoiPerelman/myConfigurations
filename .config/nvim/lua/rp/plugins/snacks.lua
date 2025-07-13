require('snacks').setup({
  picker = {
    layouts = {
      select = require("snacks.picker.config.layouts").default,
      vertico = {
        preset = "ivy",
        preview = "main",
        layout = { height = 12 },
      },
    }
  },
  notifier = { enabled = true },
  indent = { enabled = true },
  zen = {
    toggles = {
      dim = false,
      git_signs = true,
      mini_diff_signs = true,
      diagnostics = true,
      -- inlay_hints = false,
    },
    win = {
      width = 160,
      backdrop = { transparent = false },
    }
  },
  bigfile = { enabled = true },
  quickfile = { enabled = true },
})

-- use snacks debug functions for easier debugging
_G.dd = function(...)
  Snacks.debug.inspect(...)
end
_G.bt = function()
  Snacks.debug.backtrace()
end
vim.print = _G.dd

-- notifier
vim.keymap.set('n', '<leader>n', function() Snacks.notifier.show_history() end, { desc = '[N]otifications' })

-- bufdelete
vim.keymap.set('n', '<leader>bc', function() Snacks.bufdelete() end, { desc = '[B]uffer [C]lose' })
vim.keymap.set('n', '<leader>bC', function() Snacks.bufdelete.all() end, { desc = '[B]uffer [C]lose' })

-- zen
vim.keymap.set('n', '<leader>z', function() Snacks.zen() end, { desc = '[Z]en mode toggle' })

-- picker
local function get_project_root()
  -- my old implementation of buf_root made before Snacks
  -- return require("rp.utils.find_buf_root")
  return Snacks.git.get_root()
end

local function with_opts(opts)
  return vim.tbl_extend("force", {
    layout = { preset = 'vertico' },
    hidden = true,
  }, opts or {})
end

local map = vim.keymap.set
local picker = require("snacks").picker

-- files
map('n', '<leader>ff', function() picker.files(with_opts()) end, { desc = '[F]ind [F]iles cwd' })
map('n', '<leader>fF', function() picker.files(with_opts({ cwd = get_project_root() })) end, { desc = '[F]ind [F]iles buf root' })
map('n', '<leader>fc', function() picker.files(with_opts({ cwd = vim.fn.stdpath('config') })) end, { desc = '[F]ind [C]onfig files' })
map('n', '<leader>fC', function() picker.files(with_opts({ cwd = MiniDeps.config.path.package })) end, { desc = "[F]ind [C]onfig's plugin files" })
map('n', '<leader>fr', function() picker.recent(with_opts()) end, { desc = '[F]ind [R]ecent files' })
map('n', '<leader>fb', function() picker.buffers(with_opts()) end, { desc = '[F]ind [B]uffers' })

-- vim.keymap.set('n', '<leader><leader>', function()
--   Snacks.picker.smart({
--     prompt = 'Find file: ',
--     layout = {
--       preview = "main",
--       preset = "ivy",
--       layout = { height = 15 }
--     }
--   })
-- end, { desc = '[F]ind [F]iles cwd' })

-- grep
map('n', '<leader>sg', function() picker.grep(with_opts()) end, { desc = '[S]earch [G]rep' })
map('n', '<leader>sG', function() picker.grep(with_opts({ cwd = get_project_root() })) end, { desc = '[S]earch [G]rep buf root' })
map('n', '<leader>sw', function() picker.grep_word(with_opts()) end, { desc = '[S]earch [W]ord under cursor' })
map('n', '<leader>sW', function() picker.grep_word(with_opts({ cwd = get_project_root() })) end, { desc = '[S]earch [W]ord under cursor buf root' })
map('n', '<leader>sb', function() picker.lines(with_opts()) end, { desc = '[S]earch [B]uffer lines' })

-- utilities
map('n', '<leader>sr', function() picker.resume(with_opts()) end, { desc = '[S]earch [R]esume' })
map('n', '<leader>s"', function() picker.registers(with_opts()) end, { desc = '[S]earch ["] Registers' })
map('n', '<leader>s,', function() picker.marks(with_opts()) end, { desc = '[S]earch [,] Marks' })
map('n', '<leader>s/', function() picker.search_history(with_opts()) end, { desc = '[S]earch [/] Search History' })
map('n', '<leader>sc', function() picker.command_history(with_opts()) end, { desc = '[S]earch [C]ommand history' })
map('n', '<leader>sC', function() picker.commands(with_opts()) end, { desc = '[S]earch [C]ommand' })
map('n', '<leader>sa', function() picker.autocmds(with_opts()) end, { desc = '[S]earch [A]utocmds' })
map('n', '<leader>sh', function() picker.help(with_opts()) end, { desc = '[S]earch [H]elp' })
map('n', '<leader>sk', function() picker.keymaps(with_opts()) end, { desc = '[S]earch [K]eymaps' })
map('n', '<leader>sj', function() picker.jumps(with_opts()) end, { desc = '[S]earch [J]umplist' })
map('n', '<leader>sm', function() picker.man(with_opts()) end, { desc = '[S]earch [M]an' })
map('n', '<leader>sd', function() picker.diagnostics(with_opts()) end, { desc = '[S]earch [D]iagnostics' })
map('n', '<leader>sD', function() picker.diagnostics_buffer(with_opts()) end, { desc = '[S]earch [D]iagnostics in buffer' })
map('n', '<leader>su', function() picker.undo(with_opts()) end, { desc = '[S]earch [U]ndo' })
map('n', '<leader>sq', function() picker.qflist(with_opts()) end, { desc = '[S]earch [Q]uickfix' })
map('n', '<leader>sl', function() picker.loclist(with_opts()) end, { desc = '[S]earch [L]oclist' })
map('n', '<leader>si', function() picker.icons(with_opts()) end, { desc = '[S]earch [I]cons' })
map('n', '<leader>cc', function() picker.colorschemes(with_opts()) end, { desc = '[C]hange [C]olorscheme' })

-- git
map('n', '<leader>gs', function() picker.git_status(with_opts()) end, { desc = '[G]it [S]tatus' })
map('n', '<leader>gS', function() picker.git_branches(with_opts()) end, { desc = '[G]it [S]tash' })
map('n', '<leader>gl', function() picker.git_log(with_opts()) end, { desc = '[G]it [L]og' })
map('n', '<leader>gL', function() picker.git_log_file(with_opts()) end, { desc = '[G]it [L]og [F]ile' })

-- lsp
-- local map = function(keys, func, desc)
--   vim.keymap.set("n", keys, func, { desc = "LSP: " .. desc })
-- end
map("n", "gd", function() picker.lsp_definitions(with_opts()) end, { desc = "[G]oto [D]efinition" })
map("n", "gD", function() picker.lsp_declarations(with_opts()) end, { desc = "[G]oto [D]eclaration" })
map("n", "grr", function() picker.lsp_references(with_opts()) end, { desc = "[G]oto [R]elated [R]eferences" })
map("n", "gri", function() picker.lsp_implementations(with_opts()) end, { desc = "[G]oto [R]elated [I]mplementations" })
map("n", "gt", function() picker.lsp_type_definitions(with_opts()) end, { desc = "[G]oto [T]ype Definition" })
map("n", "gO", function() picker.lsp_symbols(with_opts()) end, { desc = "[G]oto Document Symbols" })
map("n", "grs", function() picker.lsp_workspace_symbols(with_opts()) end, { desc = "[G]oto [R]elated Workspace [S]ymbols" })
