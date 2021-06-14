-- bootstrap packer
local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data").."/site/pack/packer/start/packer.nvim"


if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({"git", "clone", "https://github.com/wbthomason/packer.nvim", install_path})
  execute "packadd packer.nvim"
end

--- Check if a file or directory exists in this path
--local function require_plugin(plugin)
--    local plugin_prefix = fn.stdpath("data") .. "/site/pack/packer/opt/"

--    local plugin_path = plugin_prefix .. plugin .. "/"
--    --	print('test '..plugin_path)
--    local ok, err, code = os.rename(plugin_path, plugin_path)
--    if not ok then
--        if code == 13 then
--            -- Permission denied, but it exists
--            return true
--        end
--    end
--    --	print(ok, err, code)
--    if ok then vim.cmd("packadd " .. plugin) end
--    return ok, err, code
--end

-- packer!!!
local packer = require"packer"
-- limit jobs because of home computer problem with too many.
packer.init({ max_jobs = 6 })
-- startup
packer.startup(function()
-- add local use just for lsp saying undefined global `use`
  local use = packer.use
  -- Packer can manage itself
  use "wbthomason/packer.nvim"

  -- Text manipulation
  -- Surround
  -- cs"' - change surrounding "" to ''
  -- ysiw<q> - you surround inside word with <q> <q/>
  -- dst - delete surrounding tag (for these kinds of tags <>)
  -- t=<>, b=(, B={
      -- :help ys cs or ds for more information
  use 'tpope/vim-surround'
  -- Better Comments
  use 'tpope/vim-commentary'
  -- Add repeat with '.' to other plugins
  use 'tpope/vim-repeat'
  -- Git
  -- :Gstatus
  -- s - stage
  -- u - unstage
  -- dv - start resolve conflicts
  -- :diffget //2 to choose left and :diffget //3 to choose right
  use 'tpope/vim-fugitive'
  -- gitsigns
  use 'lewis6991/gitsigns.nvim'
  -- git blame
  use 'f-person/git-blame.nvim'

  -- file explorer
  use "kyazdani42/nvim-tree.lua"

    -- Incremental Search improved (automatically clear highlights)
  -- use 'haya14busa/is.vim'
  -- Asterisk behavior change
  -- use 'haya14busa/vim-asterisk'


  -- fzf telescope
  use 'nvim-lua/plenary.nvim'
  use 'nvim-lua/popup.nvim'
  use "nvim-telescope/telescope.nvim"
  -- use "nvim-telescope/telescope-fzy-native.nvim"
  -- use "nvim-telescope/telescope-project.nvim"

  -- lsp
  -- lsp client configuration for communicating with lsp server
  use "neovim/nvim-lspconfig"
  -- lsp server installation tool inside nvim with :LspInstall <language>
  -- to use lspinstall first :PackerLoad nvim-lspinstall
  -- to see installed servers :lua print(vim.inspect(require"lspinstall".installed_servers()))
  use { "kabouzeid/nvim-lspinstall", opt = true }
  -- lua-dev for better sumneko_lua language server
  use "folke/lua-dev.nvim"
  -- A pretty list for showing diagnostics, references, telescope results, quickfix and location lists
  -- use "folke/trouble.nvim"
  -- lsp saga for code actions, signature and hover previewers
  -- use "glepnir/lspsaga.nvim"

  -- completion
  -- use "nvim-lua/completion-nvim"
  use "hrsh7th/nvim-compe"

  -- snippets
  -- use "hrsh7th/vim-vsnip"
  -- use "rafamadriz/friendly-snippets"

  -- TODO: do I want these?
    -- Status line
    -- use 'vim-airline/vim-airline'
    -- use 'vim-airline/vim-airline-themes'
  -- to create a snippet from code, use Coc action <leader>la or <leader>lA
  -- use 'honza/vim-snippets'
  -- which key
  -- use "folke/which-key.nvim"

  -- treesitter - code parser
  use {"nvim-treesitter/nvim-treesitter", run = ":TSUpdate"}
  -- color rainbow parentheses
  use 'p00f/nvim-ts-rainbow'
  -- autoclose and autorename html tag
  use 'windwp/nvim-ts-autotag'
  -- autopairs aware to context
  use 'windwp/nvim-autopairs'
  -- TODO read functionality - jump to any matching tag with %
  use 'andymass/vim-matchup'
  -- context aware commenting used with vim-commentary
  use 'JoosepAlviste/nvim-ts-context-commentstring'
  -- TODO check this out, use :PackerLoad
  use {'nvim-treesitter/playground', opt = true}

  -- Startify
  use 'mhinz/vim-startify'

  -- Vim Wiki
  use 'vimwiki/vimwiki'

  -- just some icons
  use "kyazdani42/nvim-web-devicons"

  -- indent line
  use {'lukas-reineke/indent-blankline.nvim', branch = "lua"}

  -- Auto pairs for '(' '[' '{'.
  -- use 'jiangmiao/auto-pairs'

  -- color colorizer
  use {
    "norcalli/nvim-colorizer.lua",
    config= function() require'colorizer'.setup() end
  }

  -- Syntax Support
  use 'sheerun/vim-polyglot'

  -- colorschemes
  use 'ChristianChiarulli/nvcode-color-schemes.vim'

  -- TODO check these out - all optional for now
  -- use {"Pocco81/TrueZen.nvim", opt = true}
  -- use {"glepnir/galaxyline.nvim", opt = true}
  -- use {"romgrk/barbar.nvim", opt = true}
  -- use {"kevinhwang91/nvim-bqf", opt = true}
  -- use {"ChristianChiarulli/dashboard-nvim", opt = true}
  -- use {"mfussenegger/nvim-dap", opt = true}

  -- require optional plugins
  -- require_plugin('kabouzeid/nvim-lspinstall')
  -- require_plugin('nvim-treesitter/playground')
end)
