return {
  "ibhagwan/fzf-lua",
  -- optional for icon support
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    -- calling `setup` is optional for customization
    require("fzf-lua").setup({ split = "belowright new" })

    -- vim.keymap.set("n", "<leader>ff", , { desc = "[F]ind [F]iles" })
  end,
}
