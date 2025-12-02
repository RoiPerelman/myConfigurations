vim.api.nvim_set_keymap("n", "<Space>", "<NOP>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.keymap.set("n", "<leader>co", ":e ~/.config/nvim/lua/roip/config/vscode.lua<cr>", { desc = "Whatever" })

-- search
vim.opt.ignorecase = true -- ignore case in search
vim.opt.smartcase = true  -- ignore case in search only if no capital letter

-- misc
vim.opt.cmdheight = 4
vim.opt.clipboard = "unnamedplus" -- always use the system clipboard

-- indenting (do not lose visual mode)
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

-- paste without overwriting clipboard
vim.keymap.set("v", "p", '"_dP')
-- vim.keymap.set("v", "p", 'P')

local vscode_mappings = {
  -- keywords - _ update
  { "n", "w",          "cursorWordPartRight" },
  { "n", "b",          "cursorWordPartLeft" },
  { "v", "w",          "cursorWordPartRightSelect" },
  { "v", "b",          "cursorWordPartLeftSelect" },
  -- explorer
  { "n", "<Leader>e",  "workbench.files.action.showActiveFileInExplorer" },
  -- lsp shortcuts
  { "n", "grn",        "editor.action.rename" },
  { "n", "gra",        "editor.action.quickFix" },
  { "n", "grr",        "editor.action.goToReferences" },
  { "n", "gri",        "editor.action.goToImplementation" },
  { "n", "grt",        "editor.action.goToTypeDefinition" },
  { "n", "grs",        "editor.action.gotoSymbol" },
  { "n", "cf",         "editor.action.formatDocument" },
  { "n", "co",         "editor.action.organizeImports" },
  { "n", "[d",         "editor.action.marker.next" },
  { "n", "]d",         "editor.action.marker.prev" },
  -- git hunk operations
  { "n", "<Leader>hr", "git.revertSelectedRanges" },
  { "n", "<Leader>hs", "git.stageSelectedRanges" },
  { "v", "<Leader>hs", "git.stageSelectedRanges" },
  { "v", "<Leader>hu", "git.unstageSelectedRanges" },
  { "n", "<Leader>hp", "editor.action.dirtydiff.next" },
  { "n", "]h",         "workbench.action.editor.nextChange" },
  { "n", "[h",         "workbench.action.editor.previousChange" },
  -- find file operations
  { "n", "<Leader>ff", "workbench.action.quickOpen" },
  { "n", "<Leader>fp", "workbench.action.openRecent" },
  { "n", "<Leader>fb", "workbench.action.showAllEditors" },
  { "n", "<Leader>fr", "workbench.action.openRecent" },
  { "n", "<Leader>fs", "workbench.action.openSettingsJson" },
  { "n", "<Leader>fk", "workbench.action.openGlobalKeybindingsFile" },
  { "n", "<Leader>fn", "workbench.action.files.newUntitledFile" },
  -- move lines up and down
  { "n", "<A-j>",      "editor.action.moveLinesDownAction" },
  { "n", "<A-k>",      "editor.action.moveLinesUpAction" },
  { "i", "<A-j>",      "editor.action.moveLinesDownAction" },
  { "i", "<A-k>",      "editor.action.moveLinesUpAction" },
  { "v", "<A-j>",      "editor.action.moveLinesDownAction" },
  { "v", "<A-k>",      "editor.action.moveLinesUpAction" },
  --  // close all editors in group (all buffers in window)
  { "n", "<C-w>C",     "workbench.action.closeEditorsAndGroup" },
  -- vscode specific
  { "n", "<Leader>R",  "vscode-neovim.restart" },
  -- { "n", "<Leader>tt", "workbench.action.terminal.toggleTerminal" },
}


for _, mapping in ipairs(vscode_mappings) do
  local mode, key, command = mapping[1], mapping[2], mapping[3]
  vim.keymap.set(mode, key, function() vim.fn.VSCodeNotify(command) end, { noremap = true, silent = true })
end

-- search word under cursor
vim.keymap.set(
  "n",
  "<leader>sw",
  function() require('vscode').action('workbench.action.findInFiles', { args = { query = vim.fn.expand('<cword>') } }) end,
  { noremap = true, silent = true })
-- search visual selection
vim.keymap.set(
  "v",
  "<leader>sw",
  function()
    -- Store the current visual selection before it gets lost
    local mode = vim.fn.mode()
    local start_pos = vim.fn.getpos('v')
    local end_pos = vim.fn.getpos('.')

    -- Make sure we have the right order (start should be before end)
    if start_pos[2] > end_pos[2] or (start_pos[2] == end_pos[2] and start_pos[3] > end_pos[3]) then
      start_pos, end_pos = end_pos, start_pos
    end

    local lines = vim.api.nvim_buf_get_text(0, start_pos[2] - 1, start_pos[3] - 1, end_pos[2] - 1, end_pos[3], {})
    local selected_text = table.concat(lines, '\n')

    require('vscode').action('workbench.action.findInFiles', { args = { query = selected_text or "" } })
  end,
  { noremap = true, silent = true })
