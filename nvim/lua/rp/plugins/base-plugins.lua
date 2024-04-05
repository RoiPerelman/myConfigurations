return {
  -- Text manipulation
  -- Surround
  -- cs"' - change surrounding "" to ''
  -- ysiw<q> - you surround inside word with <q> <q/>
  -- dst - delete surrounding tag (for these kinds of tags <>)
  -- t=<>, b=(, B={
  -- :help ys cs or ds for more information
  --
  "tpope/vim-surround",
  "tpope/vim-repeat",
  "tpope/vim-sleuth", -- Detect tabstop and shiftwidth automatically

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
  -- Collection of various small independent plugins/modules
  {
    "echasnovski/mini.nvim",
    config = function()
      -- Better Around/Inside textobjects
      --  - va)  - [V]isually select [A]round [)]paren
      --  - yinq - [Y]ank [I]nside [N]ext [']quote
      --  - ci'  - [C]hange [I]nside [']quote
      require("mini.ai").setup({ n_lines = 500 })

      -- Add/delete/replace surroundings (brackets, quotes, etc.)
      -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
      -- - sd'   - [S]urround [D]elete [']quotes
      -- - sr)'  - [S]urround [R]eplace [)] [']
      -- require("mini.surround").setup()

      require("mini.pairs").setup()

      local statusline = require("mini.statusline")
      -- set use_icons to true if you have a Nerd Font
      statusline.setup({ use_icons = vim.g.have_nerd_font })

      -- You can configure sections in the statusline by overriding their
      -- default behavior. For example, here we set the section for
      -- cursor location to LINE:COLUMN
      ---@diagnostic disable-next-line: duplicate-set-field
      statusline.section_location = function()
        return "%2l:%-2v"
      end
    end,
  },
  -- Highlight todo, notes, etc in comments
  {
    "folke/todo-comments.nvim",
    event = "VimEnter",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = { signs = false },
  },
  -- TODO: DECIDE if I want to remove
  -- add nvim-tree
  -- {
  -- 	"kyazdani42/nvim-tree.lua",
  -- 	config = function()
  -- 		vim.keymap.set("n", "<Leader>e", ":NvimTreeToggle<CR>")
  -- 		require("nvim-tree").setup({})
  -- 	end,
  -- },
  "kyazdani42/nvim-web-devicons",
  {
    "stevearc/oil.nvim",
    config = function()
      vim.keymap.set("n", "<Leader>e", ":Oil<CR>")
      require("oil").setup({
        view_options = {
          show_hidden = true,
        },
      })
    end,
  },
}
