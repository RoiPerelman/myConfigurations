return {
  -- add indent line
  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPre", "BufNewFile" },
    main = "ibl",
    opts = {
      indent = {
        char = "│", -- "┊"
        tab_char = "│",
      },
    },
  },

  -- Highlight todo, notes, etc in comments
  {
    "folke/todo-comments.nvim",
    cmd = { "TodoTelescope" },
    event = { "BufReadPre", "BufNewFile" },
    keys = {
      {
        "]t",
        function()
          require("todo-comments").jump_next()
        end,
        desc = "Next [T]odo Comment",
      },
      {
        "[t",
        function()
          require("todo-comments").jump_prev()
        end,
        desc = "Previous [T]odo Comment",
      },
      { "<leader>st", "<cmd>TodoTelescope<cr>", desc = "[S]earch [T]odo" },
    },
  },

  -- add rainbow delimiters
  -- {
  --   "HiPhish/rainbow-delimiters.nvim",
  --   event = { "BufReadPre", "BufNewFile" },
  --   config = function()
  --     require("rainbow-delimiters.setup").setup({})
  --   end,
  -- },
}
