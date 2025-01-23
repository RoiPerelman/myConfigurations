require("telescope").setup({
  defaults = {
    layout_config = { prompt_position = "top" },
    sorting_strategy = "ascending",
    winblend = 0,
    mappings = {
      n = {
        ["q"] = require("telescope.actions").close,
      },
    },
    pickers = {
      find_files = {
        file_ignore_patterns = { "node_modules", ".git", ".venv" },
        hidden = true,
      },
      live_grep = {
        file_ignore_patterns = { "node_modules", ".git", ".venv" },
        additional_args = function(_)
          return { "--hidden" }
        end,
      },
      -- add marks delete mark
      marks = {
        attach_mappings = function(_, map)
          map({ "i", "n" }, "<C-m>", require("telescope.actions").delete_mark)
          return true
        end,
      },
    },
    extensions = {
      ["ui-select"] = {
        require("telescope.themes").get_dropdown(),
      },
    },
  },
})

pcall(require("telescope").load_extension, "fzf")
pcall(require("telescope").load_extension, "ui-select")

vim.keymap.set("n", "<leader>ff", function()
  require("telescope.builtin").find_files({
    find_command = { "rg", "--files", "--color", "never", "-g", "!.git", "--hidden" },
  })
end, { desc = "[F]ind [F]iles cwd" })

vim.keymap.set("n", "<leader>fF", function()
  local buf_root = require("rp.utils.find_buf_root")()
  require("telescope.builtin").find_files({
    find_command = { "rg", "--files", "--color", "never", "-g", "!.git", "--hidden" },
    cwd = buf_root
  })
end, { desc = "[F]ind [F]iles buf root" })

vim.keymap.set("n", "<leader>fc", function()
  require("telescope.builtin").find_files({
    find_command = { "rg", "--files", "--color", "never", "-g", "!.git", "--hidden" },
    cwd = vim.fn.stdpath("config"),
  })
end, { desc = "[F]ind [C]onfig files" })

vim.keymap.set("n", "<leader>fC", function()
  require("telescope.builtin").find_files({
    find_command = { "rg", "--files", "--color", "never", "-g", "!.git", "--hidden" },
    cwd = MiniDeps.config.path.package,
  })
end, { desc = "[F]ind [C]onfig's plugin files" })

vim.keymap.set("n", "<leader>fr", require("telescope.builtin").oldfiles, { desc = "[F]ind [R]ecent files" })
vim.keymap.set("n", "<leader>fb", "<cmd>Telescope buffers sort_mru=true sort_lastused=true<cr>",
  { desc = "[F]ind [B]uffers" })

vim.keymap.set("n", "<leader>sg", require("telescope.builtin").live_grep, { desc = "[S]earch [G]rep" })
vim.keymap.set("n", "<leader>sw", require("telescope.builtin").grep_string, { desc = "[S]earch [W]ord under cursor" })
vim.keymap.set("n", "<leader>sx", function()
  require "telescope.builtin".grep_string({
    use_regex = true,
    search = vim.fn.input("Grep for > ")
  })
end, { desc = "[S]earch rege[X] and after live grep" })

vim.keymap.set("n", "<leader>sh", require("telescope.builtin").help_tags, { desc = "[S]earch [H]elp" })
vim.keymap.set("n", "<leader>sH", require("telescope.builtin").highlights, { desc = "[S]earch [H]ighlights" })
vim.keymap.set("n", "<leader>sd", require("telescope.builtin").diagnostics, { desc = "[S]earch [D]iagnostics" })
vim.keymap.set("n", "<leader>sq", require("telescope.builtin").quickfix, { desc = "[S]earch [Q]uickfix" })
vim.keymap.set("n", "<leader>sm", require("telescope.builtin").marks, { desc = "[S]earch [M]ark" })
vim.keymap.set("n", "<leader>sr", require("telescope.builtin").resume, { desc = "[S]earch [R]esume" })
vim.keymap.set("n", "<leader>sk", require("telescope.builtin").keymaps, { desc = "[S]earch [K]eymaps" })
vim.keymap.set("n", "<leader>sj", require("telescope.builtin").jumplist, { desc = "[S]earch [J]umplist" })
vim.keymap.set("n", "<leader>sb", require("telescope.builtin").current_buffer_fuzzy_find, { desc = "[S]earch [B]uffer" })
vim.keymap.set("n", "<leader>sc", require("telescope.builtin").command_history, { desc = "[S]earch [C]command history" })
vim.keymap.set("n", "<leader>sC", require("telescope.builtin").commands, { desc = "[S]earch [C]commands" })
vim.keymap.set("n", "<leader>so", require("telescope.builtin").vim_options, { desc = "[S]earch vim [O]ptions" })
