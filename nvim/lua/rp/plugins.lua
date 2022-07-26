-- Bootstrapping packer
local fn = vim.fn
-- Automatically install packer on initial startup
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
local packer_bootstrap = nil

if fn.empty(fn.glob(install_path)) > 0 then
	Packer_Bootstrap =
		fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
	print("---------------------------------------------------------")
	print("Press Enter to install packer and plugins.")
	print("After install -- close and reopen Neovim to load configs!")
	print("---------------------------------------------------------")
	vim.cmd([[packadd packer.nvim]])
end

vim.cmd([[
   augroup packer_user_config
      autocmd!
      autocmd BufWritePost plugins.lua source <afile> | PackerSync
   augroup end
]])

-- Use a protected call
local present, packer = pcall(require, "packer")

if not present then
	return
end
-- Have packer use a popup window

packer.init({
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
})

return packer.startup(function(use)
	use("wbthomason/packer.nvim") -- Packer can manage itself

	-- My plugins here
	use("nvim-lua/popup.nvim") -- An implementation of the Popup API from vim in Neovim
	use("nvim-lua/plenary.nvim") -- Useful lua functions used ny lots of plugins
	use("kyazdani42/nvim-tree.lua") -- file explorer
	use("kyazdani42/nvim-web-devicons") -- just some icons

	-- lsp
	use("neovim/nvim-lspconfig") -- enable lsp
	use("williamboman/nvim-lsp-installer") -- simple installation of language servers
	use("jose-elias-alvarez/null-ls.nvim") -- for formatters and linters

	-- completion
	use("hrsh7th/nvim-cmp")
	use("hrsh7th/cmp-nvim-lsp")
	use("hrsh7th/cmp-buffer")
	use("hrsh7th/cmp-path")
	use("hrsh7th/cmp-cmdline")
	use("hrsh7th/cmp-nvim-lua")
	use("saadparwaiz1/cmp_luasnip")
	use("onsails/lspkind.nvim")

	-- snippets
	use("L3MON4D3/LuaSnip") --snippet engine
	use("rafamadriz/friendly-snippets") -- a bunch of snippets to use

	-- telescope
	use("nvim-telescope/telescope.nvim")
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })

	-- treesitter - code parser
	use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })
	use("JoosepAlviste/nvim-ts-context-commentstring") -- context aware commenting used with vim-commentary
	-- use "lukas-reineke/indent-blankline.nvim" -- indent line
	-- use "windwp/nvim-autopairs" -- Autopairs, integrates with both cmp and treesitter
	-- comment

	use("numToStr/Comment.nvim")

	-- org mode!
	use("nvim-orgmode/orgmode")

	-- Text manipulation
	-- Surround
	-- cs"' - change surrounding "" to ''
	-- ysiw<q> - you surround inside word with <q> <q/>
	-- dst - delete surrounding tag (for these kinds of tags <>)
	-- t=<>, b=(, B={
	-- :help ys cs or ds for more information
	--
	use("tpope/vim-surround")
	use("tpope/vim-repeat")

	-- Git
	-- :Gstatus
	-- s - stage
	-- u - unstage
	-- dv - start resolve conflicts
	-- :diffget //2 to choose left and :diffget //3 to choose right
	use("tpope/vim-fugitive")
	use("lewis6991/gitsigns.nvim") -- gitsigns
	use("f-person/git-blame.nvim") -- git blame

	-- colorschemes
	use({ "dracula/vim", as = "dracula" })
	use({ "sainnhe/sonokai" })

  -- tmux
  use("aserowy/tmux.nvim")

	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins
	if packer_bootstrap then
		require("packer").sync()
	end
end)
