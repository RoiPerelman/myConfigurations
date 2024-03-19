return { -- You can easily change to a different colorscheme.
	-- "folke/tokyonight.nvim",
	"dracula/vim",
	priority = 1000, -- Make sure to load this before all the other start plugins.
	init = function()
		vim.cmd.colorscheme("dracula")
		vim.cmd.hi("Comment gui=none")
	end,
}
