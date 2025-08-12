-- Clone 'mini.nvim' manually in a way that it gets managed by 'mini.deps'
local path_package = vim.fn.stdpath("data") .. "/site/"
local mini_path = path_package .. "pack/deps/start/mini.nvim"
if not vim.loop.fs_stat(mini_path) then
  vim.cmd('echo "Installing `mini.nvim`" | redraw')
  local clone_cmd = {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/echasnovski/mini.nvim",
    mini_path,
  }
  vim.fn.system(clone_cmd)
  vim.cmd("packadd mini.nvim | helptags ALL")
  vim.cmd('echo "Installed `mini.nvim`" | redraw')
end

-- Set up 'mini.deps' (customize to your liking)
require("mini.deps").setup({ path = { package = path_package } })

-- Plugins
local add = MiniDeps.add

-- colorscheme
add({ source = "folke/tokyonight.nvim" })
-- pre (other plugins use them)
add({ source = "nvim-lua/plenary.nvim" })
add({ source = "MunifTanjim/nui.nvim" })
-- lua dev
add({ source = "folke/lazydev.nvim" })
add({ source = "Bilal2453/luvit-meta" })
-- file explorer
add({ source = "stevearc/oil.nvim" })
-- tmux
add({ source = "alexghergh/nvim-tmux-navigation" })
-- treesitter
add({ source = "windwp/nvim-ts-autotag" })
add({ source = "nvim-treesitter/nvim-treesitter", hooks = { post_install = function() vim.cmd('TSUpdate') end } })
add({ source = "nvim-treesitter/nvim-treesitter-textobjects" })
add({ source = "folke/ts-comments.nvim" })
-- snacks
add({ source = "folke/snacks.nvim" })
-- git
add({ source = "tpope/vim-fugitive" })
add({ source = "lewis6991/gitsigns.nvim" })
add({ source = "sindrets/diffview.nvim" })
add({ source = "NeogitOrg/neogit" })
-- snippets
add({
  source = "L3MON4D3/LuaSnip",
  checkout = "v2.4.0",
  hooks = {
    post_checkout = function()
      vim.fn.system('make install_jsregexp')
    end,
  },
})
add({ source = "rafamadriz/friendly-snippets" })
-- blink
add({ source = "saghen/blink.cmp", checkout = "v1.1.1" })
add({ source = "alexandre-abrioux/blink-cmp-npm.nvim" })
-- copilot start with :Copilot setup
add({ source = "github/copilot.vim" })
-- language server tools
add({ source = "williamboman/mason.nvim" })
add({ source = "WhoIsSethDaniel/mason-tool-installer.nvim" })
add({ source = "neovim/nvim-lspconfig" })
add({ source = "williamboman/mason-lspconfig.nvim" })
add({ source = "mfussenegger/nvim-lint" })
add({ source = "stevearc/conform.nvim" })
