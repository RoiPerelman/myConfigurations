-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
--
require("telescope").setup({
	defaults = {
		vimgrep_arguments = {
			"rg",
			"--follow",
			"--color=never",
			"--no-heading",
			"--with-filename",
			"--line-number",
			"--column",
			"--smart-case",
			"--trim", -- add this value
		},
		sorting_strategy = "ascending",
		layout_config = {
			prompt_position = "top",
		},
	},
	pickers = {
		find_files = {
			-- find_command = { "fd", "--type", "f", "--strip-cwd-prefix", "--follow", "--exclude .git" },
			-- find_command = { "fd", "--type", "f", "--strip-cwd-prefix", "--follow" },
			find_command = { "fd", "--type", "f", "--follow" },
		},
	},
})
require("telescope").load_extension("fzf")
