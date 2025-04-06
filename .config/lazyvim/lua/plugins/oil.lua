-- stylua: ignore
if true then return {} end

return {
  "stevearc/oil.nvim",
  dependencies = { { "echasnovski/mini.icons", opts = {} } },
  opts = {
    view_options = {
      show_hidden = true,
    },
    keymaps = {
      ["<C-v>"] = "actions.select_vsplit",
    },
  },
  keys = {
    { "<Leader>o", ":Oil<CR>", mode = { "n" }, desc = "Explorer Oil (Root Dir)" },
  },
}
