return {
  "ibhagwan/fzf-lua",
  -- optional for icon support
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    -- calling `setup` is optional for customization
    local fzf = require("fzf-lua")
    -- fzf.setup({ winopts = { split = "botright 12new", preview = { default = false } } })
    fzf.setup()

    vim.keymap.set("n", "<leader>ff", function(opts)
      MyPreviewer = require("rp_preview").get_fzf_lua_previewer()

      fzf.files({
        winopts = { split = "botright 12new" },
        previewer = MyPreviewer,
      })
    end, { desc = "[F]ind [F]iles" })

    vim.keymap.set("n", "<leader>fg", function(opts)
      fzf.live_grep({
        winopts = { split = "botright 12new" },
        previewer = MyPreviewer,
      })
    end, { desc = "[F]ind [F]iles" })
  end,
}
