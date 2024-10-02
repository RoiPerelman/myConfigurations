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

-- inspekto

local inspekto_filepaths = {
  "~/tiny_inspektor/cicd/tinybox",
  "~/tiny_inspektor/cicd/ispekto-os",
  "~/tiny_inspektor/cicd/meta-inspekto",
  "~/tiny_inspektor/sw/fixi",
  "~/tiny_inspektor/sw/fixi_client",
  "~/tiny_inspektor/sw/tiny_database",
  "~/tiny_inspektor/sw/tier2/tiny_std",
  "~/tiny_inspektor/sw/tier2/common_std",
  "~/tiny_inspektor/sw/data_coordinator",
  "~/tiny_inspektor/sw/connectivity",
  "~/tiny_inspektor/sw/inspekto_agent",
  "~/tiny_inspektor/sw/integration_managers",
  "~/tiny_inspektor/sw/cv/agc",
  "~/tiny_inspektor/sw/profile_center",
  "~/tiny_inspektor/sw/inspekto_agent",
  "~/tiny_inspektor/sw/deep/defect_predictor",
  "~/tiny_inspektor/sw/deep/defect_detector",

  "~/inspekto/tinybox",
  "~/inspekto/inspekto-os",
  "~/inspekto/meta-inspekto",
  "~/inspekto/fixi",
  "~/inspekto/fixi_client",
  "~/inspekto/tiny_database",
  "~/inspekto/tier2/tiny_std",
  "~/inspekto/tier2/common_std",
  "~/inspekto/data_coordinator",
  "~/inspekto/connectivity",
  "~/inspekto/inspekto_agent",
  "~/inspekto/integration_managers",
  "~/inspekto/agc",
  "~/inspekto/profile_center",
  "~/inspekto/inspekto_agent",
  "~/inspekto/defect_predictor",
  "~/inspekto/defect_detector",
}

local inspekto_path_display = function(_, path)
  -- Define the subpath to start displaying from
  local sw_subpath_start = "sw/"
  local cicd_subpath_start = "cicd/"
  local inspekto_subpath_start = "inspekto/"
  -- Find the position where this subpath starts
  local sw_start_pos = path:find(sw_subpath_start)
  local cicd_start_pos = path:find(cicd_subpath_start)
  local inspekto_start_pos = path:find(inspekto_subpath_start)
  if sw_start_pos then
    -- Adjust the path to start from the end of the specified subpath
    return path:sub(sw_start_pos + #sw_subpath_start)
  elseif cicd_start_pos then
    -- Same for cicd subpath
    return path:sub(cicd_start_pos + #cicd_subpath_start)
  elseif inspekto_subpath_start then
    -- Same for cicd subpath
    return path:sub(inspekto_start_pos + #inspekto_subpath_start)
  end
  -- If the specific subpath isn't found, return the whole path
  return path
end

vim.keymap.set("n", "<leader>if", function()
  require("telescope.builtin").find_files({
    find_command = { "rg", "--files", "--color", "never", "-g", "!.git", "--hidden" },
    search_dirs = inspekto_filepaths,
    path_display = inspekto_path_display,
  })
end, { desc = "[I]inspekto [F]iles" })

vim.keymap.set("n", "<leader>ig", function()
  require("telescope.builtin").live_grep({
    search_dirs = inspekto_filepaths,
    path_display = inspekto_path_display,
  })
end, { desc = "[I]inspekto [G]rep" })

vim.keymap.set("n", "<leader>iw", function()
  require("telescope.builtin").grep_string({
    search_dirs = inspekto_filepaths,
    path_display = inspekto_path_display,
  })
end, { desc = "[I]inspekto [W]ord under cursor" })

vim.keymap.set("n", "<leader>ix", function()
  require "telescope.builtin".grep_string({
    use_regex = true,
    search = vim.fn.input("Grep for > "),
    search_dirs = inspekto_filepaths,
    path_display = inspekto_path_display,
  })
end, { desc = "[S]earch rege[X] and after live grep" })
