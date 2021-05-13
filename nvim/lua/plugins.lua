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
  use "nvim-lua/completion-nvim"

  -- Text manipulation
  -- comments
  use "tpope/vim-commentary"

  -- fzf telescope
  use {
    "nvim-telescope/telescope.nvim",
    requires = {{"nvim-lua/popup.nvim"}, {"nvim-lua/plenary.nvim"}}
  }

  -- which key
  use "folke/which-key.nvim"

  -- treesitter - code parser (used in colorschemes)
  use {"nvim-treesitter/nvim-treesitter", run = ":TSUpdate"}
  -- TODO check this out
  -- use 'nvim-treesitter/playground'


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
