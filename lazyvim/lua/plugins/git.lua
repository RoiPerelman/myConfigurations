return {
  -- Gdiff commands - Gwrite, Gread, diffget (dg), diffput (do which stands for obtain), diffupdate
  -- in merge: left - 2 is target(head), mid - 1 working copy to change, right - 3 is merge
  -- dp works but do doesn't. diffget //2 or diffget //3. u can add | diffupdate at the end
  {
    "tpope/vim-fugitive",
    cmd = { "Git", "Gedit", "Gdiff" },
    keys = {
      { "<leader>gb", ":Git blame<CR>", mode = { "n" }, desc = "Git Blame Buffer" },
    },
  },
  { "sindrets/diffview.nvim" },
}
