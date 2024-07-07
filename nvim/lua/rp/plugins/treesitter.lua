-- TODO: `:help nvim-treesitter`
-- Highlight, edit, and navigate code
return {
  "nvim-treesitter/nvim-treesitter",
  event = { "BufReadPre", "BufNewFile" },
  build = ":TSUpdate",
  dependencies = {
    "windwp/nvim-ts-autotag",
  },
  config = function()
    require("nvim-treesitter.configs").setup({
      ensure_installed = {
        "bash",
        "c",
        "html",
        "lua",
        "markdown",
        "markdown_inline",
        "vim",
        "vimdoc",
        "javascript",
        "typescript",
        "python",
        "dockerfile",
        "json",
        "jsonc",
        "regex",
      },
      auto_install = true,
      highlight = {
        enable = true,
      },
      indent = { enable = true },
      -- enable autotagging (w/ nvim-ts-autotag plugin)
      autotag = {
        enable = true,
      },
      -- TODO: check this out
      -- incremental_selection = {
      --   enable = true,
      --   keymaps = {
      --     init_selection = "<Leader>is",
      --     node_incremental = "<Leader>is",
      --     scope_incremental = false,
      --     node_decremental = "<bs>",
      --   },
      -- },
    })

    -- There are additional nvim-treesitter modules that you can use to interact
    -- with nvim-treesitter. You should go explore a few and see what interests you:
    --
    --    - Incremental selection: Included, see `:help nvim-treesitter-incremental-selection-mod`
    --    - Show your current context: https://github.com/nvim-treesitter/nvim-treesitter-context
    --    - Treesitter + textobjects: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
  end,
}
