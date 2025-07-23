require("oil").setup({
  keymaps = {
    ["<C-R>"] = "actions.refresh",
    ["<C-l>"] = false,
    ["<C-h>"] = false,
  },
  view_options = {
    show_hidden = true,
    is_always_hidden = function(name, _)
      return name == '..' or name == '.git'
    end,
  }
})

vim.keymap.set("n", "<leader>e", ":Oil<CR>", { desc = "[E]xplorer Oil" })
