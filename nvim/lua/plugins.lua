-- bootstrap packer
local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data").."/site/pack/packer/start/packer.nvim"


if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({"git", "clone", "https://github.com/wbthomason/packer.nvim", install_path})
  execute "packadd packer.nvim"
end

-- add local use just for lsp saying undefined global `use`
local use = require("packer").use
return require("packer").startup(function()
  -- Packer can manage itself
  use "wbthomason/packer.nvim"



  -- lsp
  -- lsp client configuration for communicating with lsp server
  use "neovim/nvim-lspconfig"
  -- lsp server installation tool inside nvim with :LspInstall <language>
  -- to use lspinstall first :PackerLoad nvim-lspinstall
  -- to see installed servers :lua print(vim.inspect(require"lspinstall".installed_servers()))
  use { "kabouzeid/nvim-lspinstall", opt = true }

  -- completion
  -- use "nvim-lua/completion-nvim"
  use "hrsh7th/nvim-compe"

  -- Text manipulation
  -- Surround
  -- cs"' - change surrounding "" to ''
  -- ysiw<q> - you surround inside word with <q> <q/>
  -- dst - delete surrounding tag (for these kinds of tags <>)
  -- t=<>, b=(, B={
  -- :help ys cs or ds for more information
  use 'tpope/vim-surround'
  -- Better Comments
  -- gcc - go comment current
  -- gcap - go comment a paragraph
  use 'tpope/vim-commentary'
  -- Add repeat with '.' to other plugins
  use 'tpope/vim-repeat'

  -- fzf telescope
  use {
    "nvim-telescope/telescope.nvim",
    requires = {{"nvim-lua/popup.nvim"}, {"nvim-lua/plenary.nvim"}}
  }

  -- file explorer
  use "kyazdani42/nvim-tree.lua"

  -- TODO: do I want it?
  -- which key
  -- use "folke/which-key.nvim"

  -- treesitter - code parser (used in colorschemes)
  use {"nvim-treesitter/nvim-treesitter", run = ":TSUpdate"}
  -- TODO check this out
  -- use 'nvim-treesitter/playground'

  -- color colorizer
  use {
    "norcalli/nvim-colorizer.lua",
    -- config= function() require'colorizer'.setup() end
  }

  -- just some icons
  use "kyazdani42/nvim-web-devicons"

  -- colorschemes
  -- kosmikoa
  use {
    "novakne/kosmikoa.nvim",
    branch = "main",
    config = function()
      require"kosmikoa".setup()
    end,
  }

end)
