local add = MiniDeps.add

add({
  source = "nvim-telescope/telescope-fzf-native.nvim",
  hooks = {
    post_install = function(args)
      -- Check if "make" is executable
      if vim.fn.executable("make") == 1 then
        -- Run "make" in the plugin's directory
        -- vim.cmd("silent !cd " .. args.path .. " && make")
        -- Use vim.fn.system to run "make" in the plugin directory
        local result = vim.fn.system({ 'make', '-C', args.path })

        -- Optionally handle errors
        if vim.v.shell_error ~= 0 then
          print("telescope-fzf-native post_install error: " .. result)
        else
          print("telescope-fzf-native post_install success")
        end
      else
        print("telescope-fzf-native post_install make error - missing executable")
      end
    end,
  },
})
add({
  source = "nvim-telescope/telescope-ui-select.nvim",
})
add({
  source = "nvim-telescope/telescope.nvim",
})

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

local builtin = require("telescope.builtin")

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
end, { desc = "[F]ind [C]onfig plugin files" })

vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "[F]ind [H]elp" })
vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "[F]ind live [G]rep" })
vim.keymap.set("n", "<leader>fw", builtin.grep_string, { desc = "[F]ind current [W]ord and after live grep" })
vim.keymap.set("n", "<leader>fd", builtin.diagnostics, { desc = "[F]ind [D]iagnostics" })
vim.keymap.set("n", "<leader>fe", builtin.oldfiles, { desc = "[F]ind Recent [E]dited Files" })
vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "[F]ind [B]uffers" })
vim.keymap.set("n", "<leader>fq", builtin.quickfix, { desc = "[F]ind [Q]uickfix" })
vim.keymap.set("n", "<Leader>fm", builtin.marks, { desc = "[F]ind [M]arks" })
vim.keymap.set("n", "<Leader>fj", ":Telescope jumplist<CR>", { desc = "[F]ind [J]umplist" })
vim.keymap.set(
  "n",
  "<Leader>fr",
  ':lua require"telescope.builtin".grep_string({ use_regex = true, search = vim.fn.input("Grep for > ")})<CR>',
  { desc = "[F]ind [R]egex and after live grep" }
)

-- Shortcut for searching your Neovim configuration files
vim.keymap.set("n", "<leader>fp", function()
  builtin.find_files({ cwd = vim.fn.stdpath("config") })
end, { desc = "[F]ind files in [P]rivate config" })

-- shortcut for searching all Neovim plugins
vim.keymap.set("n", "<space>fa", function()
  ---@diagnostic disable-next-line: param-type-mismatch
  builtin.find_files({ cwd = vim.fs.joinpath(vim.fn.stdpath("data"), "lazy") })
end, { desc = "[F]ind files in [A]ll plugins" })

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
