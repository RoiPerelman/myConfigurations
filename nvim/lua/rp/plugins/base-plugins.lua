return {
  -- lua functions that many plugins use
  "nvim-lua/plenary.nvim",
  -- icons that many plugins use
  "kyazdani42/nvim-web-devicons",
  -- Text manipulation
  -- Surround
  -- cs"' - change surrounding "" to ''
  -- ysiw<q> - you surround inside word with <q> <q/>
  -- dst - delete surrounding tag (for these kinds of tags <>)
  -- t=<>, b=(, B={
  -- :help ys cs or ds for more information
  --
  -- TODO: do I need/prefer there plugins?
  -- "tpope/vim-surround",
  -- "tpope/vim-repeat",
  -- "tpope/vim-sleuth", -- Detect tabstop and shiftwidth automatically
  {
    "kylechui/nvim-surround",
    event = { "BufReadPre", "BufNewFile" },
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    config = true,
  },
  {
    -- "gc" or "gb" to comment visual regions/lines
    "numToStr/Comment.nvim",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "JoosepAlviste/nvim-ts-context-commentstring",
    },
    config = function()
      require("ts_context_commentstring").setup({
        enable_autocmd = false,
      })
      -- pre hook for commenting tsx, jsx, svelte, html files
      require("Comment").setup({
        pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
      })
    end,
  },
  {
    "mbbill/undotree",

    config = function()
      vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)
    end,
  },
}
