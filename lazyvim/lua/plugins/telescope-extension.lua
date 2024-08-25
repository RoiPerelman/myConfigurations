local inspekto_filepaths = {
  "~/tiny_inspektor/cicd/tinybox",
  "~/tiny_inspektor/cicd/inspekto-os",
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

-- change some telescope options
return {
  "nvim-telescope/telescope.nvim",
  keys = {
    -- -- disable find buffers duplicate. use <leader>fb
    -- { "<leader>,", false },
    -- -- disable searchlive grep shortuct for <leader>sg
    -- { "<leader>/", false },
    -- -- disable live grep shortuct for <leader>sg
    -- { "<leader>:", false },
    -- -- diable find files duplicate. use <leader>ff
    -- { "<leader><space>", false },
    {
      "<leader>fC",
      function()
        require("telescope.builtin").find_files({
          cwd = vim.fs.joinpath(require("lazy.core.config").options.root, "LazyVim"),
        })
      end,
      desc = "[F]ind Lazyvim [C]onfig",
    },
    {
      "<leader>sx",
      ':lua require"telescope.builtin".grep_string({ use_regex = true, search = vim.fn.input("Grep for > ")})<CR>',
      desc = "[S]earch rege[x] and after live grep",
    },
    {
      "<leader>fP",
      function()
        require("telescope.builtin").find_files({
          cwd = require("lazy.core.config").options.root,
          -- cwd = vim.fs.joinpath(vim.fn.stdpath("data"), "lazy"),
        })
      end,
      desc = "[F]ind [P]lugin Files",
    },
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
      desc = "[S]earch rege[x] and after live grep",
    },
  },
  -- change some options
  opts = {
    defaults = {
      layout_strategy = "horizontal",
      layout_config = { prompt_position = "top" },
      sorting_strategy = "ascending",
      winblend = 0,
    },
    pickers = {
      marks = {
        attach_mappings = function(_, map)
          map({"i", "n"}, "<C-m>", require("telescope.actions").delete_mark)
          return true
        end,
      },
    },
  },
}
