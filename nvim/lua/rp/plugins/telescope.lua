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

return { -- Fuzzy Finder (files, lsp, etc)
  "nvim-telescope/telescope.nvim",
  cmd = "Telescope",
  dependencies = {
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      -- `cond` is used to determine whether this plugin should be installed and loaded.
      cond = function()
        return vim.fn.executable("make") == 1
      end,
    },
    { "nvim-telescope/telescope-ui-select.nvim" },
  },
  opts = {
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
  },
  config = function(_, opts)
    require("telescope").setup(opts)
    -- Enable Telescope extensions
    pcall(require("telescope").load_extension, "fzf")
    pcall(require("telescope").load_extension, "ui-select")
  end,
  keys = {
    -- find
    {
      "<leader>ff",
      function()
        require("telescope.builtin").find_files({
          find_command = { "rg", "--files", "--color", "never", "-g", "!.git", "--hidden" },
        })
      end,
      desc = "[F]ind [F]iles (cwd)",
    },
    {
      "<leader>fF",
      function()
        local buf_root = require("rp.utils").find_buf_root()
        require("telescope.builtin").find_files({
          find_command = { "rg", "--files", "--color", "never", "-g", "!.git", "--hidden" },
          cwd = buf_root,
        })
      end,
      desc = "[F]ind [F]iles (buf git root)",
    },
    { "<leader>fr", require("telescope.builtin").oldfiles, desc = "[F]ind [R]ecent files" },
    { "<leader>fb", "<cmd>Telescope buffers sort_mru=true sort_lastused=true<cr>", desc = "[F]ind [B]uffers" },
    {
      "<leader>fc",
      function()
        require("telescope.builtin").find_files({
          cwd = vim.fn.stdpath("config"),
        })
      end,
      desc = "[F]ind [C]onfig files",
    },
    {
      "<leader>fp",
      function()
        require("telescope.builtin").find_files({
          cwd = require("lazy.core.config").options.root,
        })
      end,
      desc = "[F]ind [P]lugin files",
    },
    -- search
    { "<leader>sg", require("telescope.builtin").live_grep, desc = "[S]earch [G]rep" },
    { "<leader>sw", require("telescope.builtin").grep_string, desc = "[S]earch [W]ord under cursor" },
    { "<leader>sw", require("telescope.builtin").grep_string, mode = "v", desc = "[S]earch [W]ord under cursor" },
    {
      "<leader>sx",
      ':lua require"telescope.builtin".grep_string({ use_regex = true, search = vim.fn.input("Grep for > ")})<CR>',
      desc = "[S]earch rege[X] and after live grep",
    },
    { "<leader>sh", require("telescope.builtin").help_tags, desc = "[S]earch [H]elp" },
    { "<leader>sd", require("telescope.builtin").diagnostics, desc = "[S]earch [D]iagnostics" },
    { "<leader>sq", require("telescope.builtin").quickfix, desc = "[S]earch [Q]uickfix" },
    { "<leader>sm", require("telescope.builtin").marks, desc = "[S]earch [M]ark" },
    { "<leader>sr", require("telescope.builtin").resume, desc = "[S]earch [R]esume" },
    { "<leader>sk", require("telescope.builtin").keymaps, desc = "[S]earch [K]eymaps" },
    { "<leader>sj", require("telescope.builtin").jumplist, desc = "[S]earch [J]umplist" },
    { "<leader>sb", require("telescope.builtin").current_buffer_fuzzy_find, desc = "[S]earch [B]uffer" },
    { "<leader>sc", require("telescope.builtin").command_history, desc = "[S]earch [C]command history" },
    { "<leader>sC", require("telescope.builtin").commands, desc = "[S]earch [C]commands" },
    { "<leader>so", require("telescope.builtin").vim_options, desc = "[S]earch vim [O]ptions" },
    -- inspekto
    {
      "<leader>if",
      function()
        require("telescope.builtin").find_files({
          search_dirs = inspekto_filepaths,
          additional_args = { "--hidden" },
          path_display = inspekto_path_display,
        })
      end,
      desc = "[I]nspekto [F]iles",
    },
    {
      "<leader>ig",
      function()
        require("telescope.builtin").live_grep({
          search_dirs = inspekto_filepaths,
          path_display = inspekto_path_display,
          -- additional_args = { "--hidden", "--ignore-case" }, -- Specified as a table directly
        })
      end,
      desc = "[I]nspekto [G]rep",
    },
    {
      "<leader>iw",
      function()
        require("telescope.builtin").grep_string({
          search_dirs = inspekto_filepaths,
          path_display = inspekto_path_display,
          -- additional_args = { "--hidden", "--ignore-case" }, -- Specified as a table directly
        })
      end,
      desc = "[I]nspekto [W]ord",
    },
    {
      "<leader>ix",
      ':lua require"telescope.builtin".grep_string({ use_regex = true, search = vim.fn.input("Grep for > "), search_dirs = inspekto_filepaths, path_display = inspekto_path_display})<CR>',
      desc = "[I]nspekto rege[x] and after live grep",
    },
  },
}
