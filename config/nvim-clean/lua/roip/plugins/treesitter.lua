vim.pack.add({
  {
    src = "https://github.com/nvim-treesitter/nvim-treesitter",
    version = "master",
  },
})

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
  highlight = { enable = true, additional_vim_regex_highlighting = false },
  indent = { enable = true },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<Enter>", -- set to `false` to disable one of the mappings
      node_incremental = "<Enter>",
      scope_incremental = false,
      node_decremental = "<Backspace>",
    },
  },
})

vim.api.nvim_create_autocmd('PackChanged', {
  desc = 'Handle nvim-treesitter updates',
  group = vim.api.nvim_create_augroup('nvim-treesitter-pack-changed-update-handler', { clear = true }),
  callback = function(event)
    if event.data.kind == 'update' and event.data.spec.name == 'nvim-treesitter' then
      vim.notify('nvim-treesitter updated, running TSUpdate...', vim.log.levels.INFO)
      ---@diagnostic disable-next-line: param-type-mismatch
      local ok = pcall(vim.cmd, 'TSUpdate')
      if ok then
        vim.notify('TSUpdate completed successfully!', vim.log.levels.INFO)
      else
        vim.notify('TSUpdate command not available yet, skipping', vim.log.levels.WARN)
      end
    end
  end,
})
