local add = MiniDeps.add
add({ source = "windwp/nvim-ts-autotag" })
add({ source = "nvim-treesitter/nvim-treesitter" })

-- There are additional nvim-treesitter modules that you can use to interact
-- with nvim-treesitter. You should go explore a few and see what interests you:
--
--    - Incremental selection: Included, see `:help nvim-treesitter-incremental-selection-mod`
--    - textobjects
--    - Show your current context: https://github.com/nvim-treesitter/nvim-treesitter-context
--    - Treesitter + textobjects: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
--    - nvim-ts-autotag

require("nvim-treesitter.configs").setup({
  ensure_installed = {
    "bash",
    "c",
    "dockerfile",
    "diff",
    "html",
    "javascript",
    "json",
    "jsonc",
    "lua",
    "luadoc",
    "luap",
    "markdown",
    "markdown_inline",
    "python",
    "regex",
    "toml",
    "tsx",
    "typescript",
    "vim",
    "vimdoc",
    "yaml",
  },
  opts_extend = { "ensure_installed" },
  auto_install = true,
  highlight = { enable = true },
  indent = { enable = true },
  -- enable autotagging (w/ nvim-ts-autotag plugin)
  autotag = {
    enable = true,
  },
  -- incremental_selection = {
  --   enable = true,
  --   keymaps = {
  --     init_selection = "<C-space>",
  --     node_incremental = "<C-space>",
  --     scope_incremental = false,
  --     node_decremental = "<bs>",
  --   },
  -- },
  -- textobjects = {
  --   move = {
  --     enable = true,
  --     goto_next_start = { ["]f"] = "@function.outer", ["]c"] = "@class.outer", ["]a"] = "@parameter.inner" },
  --     goto_next_end = { ["]F"] = "@function.outer", ["]C"] = "@class.outer", ["]A"] = "@parameter.inner" },
  --     goto_previous_start = { ["[f"] = "@function.outer", ["[c"] = "@class.outer", ["[a"] = "@parameter.inner" },
  --     goto_previous_end = { ["[F"] = "@function.outer", ["[C"] = "@class.outer", ["[A"] = "@parameter.inner" },
  --   },
  -- },
})
