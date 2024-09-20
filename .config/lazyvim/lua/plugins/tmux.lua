return {
  "alexghergh/nvim-tmux-navigation",
  event = "VimEnter",
  keys = {
    { "<C-h>", "<Cmd>NvimTmuxNavigateLeft<CR>", desc = "Go to Left Window" },
    { "<C-j>", "<Cmd>NvimTmuxNavigateDown<CR>", desc = "Go to Lower Window" },
    { "<C-k>", "<Cmd>NvimTmuxNavigateUp<CR>", desc = "Go to Upper Window" },
    { "<C-l>", "<Cmd>NvimTmuxNavigateRight<CR>", desc = "Go to Right Window" },
  },
  opts = {},
}
