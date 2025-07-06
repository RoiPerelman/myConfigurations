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
vim.keymap.set('n', '<leader>sG', function() Snacks.picker.grep({ cwd = get_project_root() }) end, { desc = '[S]earch [G]rep buf root' })
vim.keymap.set('n', '<leader>sw', function() Snacks.picker.grep_word() end, { desc = '[S]earch [W]ord under cursor' })
vim.keymap.set('n', '<leader>sW', function() Snacks.picker.grep_word({ cwd = get_project_root() }) end, { desc = '[S]earch [W]ord under cursor buf root' })
vim.keymap.set('n', '<leader>sb', function() Snacks.picker.lines() end, { desc = '[S]earch [B]uffer lines' })

-- utilities
vim.keymap.set('n', '<leader>sr', function() Snacks.picker.resume() end, { desc = '[S]earch [R]esume' })
vim.keymap.set('n', '<leader>s"', function() Snacks.picker.registers() end, { desc = '[S]earch ["] Registers' })
vim.keymap.set('n', '<leader>s,', function() Snacks.picker.marks() end, { desc = '[S]earch [,] Marks' })
vim.keymap.set('n', '<leader>s/', function() Snacks.picker.search_history() end, { desc = '[S]earch [/] Search History' })
vim.keymap.set('n', '<leader>sc', function() Snacks.picker.command_history() end, { desc = '[S]earch [C]ommand history' })
vim.keymap.set('n', '<leader>sC', function() Snacks.picker.commands() end, { desc = '[S]earch [C]ommand' })
vim.keymap.set('n', '<leader>sa', function() Snacks.picker.autocmds() end, { desc = '[S]earch [A]utocmds' })
vim.keymap.set('n', '<leader>sh', function() Snacks.picker.help() end, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sk', function() Snacks.picker.keymaps() end, { desc = '[S]earch [K]eymaps' })
vim.keymap.set('n', '<leader>sj', function() Snacks.picker.jumps() end, { desc = '[S]earch [J]umplist' })
vim.keymap.set('n', '<leader>sm', function() Snacks.picker.man() end, { desc = '[S]earch [M]an' })
vim.keymap.set('n', '<leader>sd', function() Snacks.picker.diagnostics() end, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>sD', function() Snacks.picker.diagnostics_buffer() end, { desc = '[S]earch [D]iagnostics in buffer' })
vim.keymap.set('n', '<leader>su', function() Snacks.picker.undo() end, { desc = '[S]earch [U]ndo' })
vim.keymap.set('n', '<leader>sq', function() Snacks.picker.qflist() end, { desc = '[S]earch [Q]uickfix' })
vim.keymap.set('n', '<leader>sl', function() Snacks.picker.loclist() end, { desc = '[S]earch [L]oclist' })
vim.keymap.set('n', '<leader>si', function() Snacks.picker.icons() end, { desc = '[S]earch [I]cons' })
vim.keymap.set('n', '<leader>cc', function() Snacks.picker.colorschemes() end, { desc = '[C]hange [C]olorscheme' })

-- git
vim.keymap.set('n', '<leader>gs', function() Snacks.picker.git_status() end, { desc = '[G]it [S]tatus' })
vim.keymap.set('n', '<leader>gS', function() Snacks.picker.git_status() end, { desc = '[G]it [S]tash' })
vim.keymap.set('n', '<leader>gl', function() Snacks.picker.git_log() end, { desc = '[G]it [L]og' })
vim.keymap.set('n', '<leader>gL', function() Snacks.picker.git_log_file() end, { desc = '[G]it [L]og [F]ile' })

-- lsp
local map = function(keys, func, desc)
  vim.keymap.set("n", keys, func, { desc = "LSP: " .. desc })
end
map("gd", function() Snacks.picker.lsp_definitions() end, "[G]oto [D]efinition")
map("gD", function() Snacks.picker.lsp_declarations() end, "[G]oto [D]eclaration")
map("grr", function() Snacks.picker.lsp_references() end, "[G]et [R]elated [R]eferences")
map("gri", function() Snacks.picker.lsp_implementations() end, "[G]et [R]elated [I]mplementations")
map("gt", function() Snacks.picker.lsp_type_definitions() end, "[G]oto [T]ype Definition")
map("gO", function() Snacks.picker.lsp_symbols() end, "[G]oto Document Symbols")
map("grs", function() Snacks.picker.lsp_workspace_symbols() end, "[G]oto [R]elated Workspace [S]ymbols")
