return {
  "gbprod/yanky.nvim",
  keys = {
    { "p", "\"_d<Plug>(YankyPutBefore)", mode = { "x" }, desc = "Put Text After Cursor" },
    { "<c-n>", "<Plug>(YankyNextEntry)", desc = "Cycle Forward Through Yank History" },
    { "<c-p>", "<Plug>(YankyPreviousEntry)", desc = "Cycle Backward Through Yank History" },
    -- disable keys
    { "]p", false },
    { "[p", false },
    { "]P", false },
    { "[P", false },
    { ">p", false },
    { "<p", false },
    { ">P", false },
    { "<P", false },
    { "=p", false },
    { "=P", false },
  },
}
