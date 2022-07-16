require("nvim-treesitter.configs").setup({
	-- ensure_installed = "maintained",
	-- colorscheme
	highlight = {
		enable = true,
		additional_vim_regex_highlighting = { "org" }, -- Required for spellcheck, some LaTex highlights and code block highlights that do not have ts grammar
	},
	-- context commentstring
	context_commentstring = {
		enable = true,
	},
	ensure_installed = { "org" }, -- Or run :TSUpdate org
})
