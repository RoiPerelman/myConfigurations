return {
	"mfussenegger/nvim-dap",
	dependencies = {
		{
			"rcarriga/nvim-dap-ui",
			dependencies = {
				"nvim-neotest/nvim-nio",
			},
		},
		"mfussenegger/nvim-dap-python",
	},
	config = function()
		local dap = require("dap")
		-- add a python language adapter
		require("dap-python").setup()

		-- add a ui for the debugger
		local dapui = require("dapui")
		require("dapui").setup()
		dap.listeners.before.attach.dapui_config = function()
			dapui.open()
		end
		dap.listeners.before.launch.dapui_config = function()
			dapui.open()
		end
		dap.listeners.before.event_terminated.dapui_config = function()
			dapui.close()
		end
		dap.listeners.before.event_exited.dapui_config = function()
			dapui.close()
		end

		local map = function(keys, func, desc)
			vim.keymap.set("n", keys, func, { desc = "DEBUGGER: " .. desc })
		end
		map("<Leader>dr", require("dap").continue, "[D]ebugger [R]un")
		map("<Leader>dsn", require("dap").step_over, "[D]ebugger Step [N]ext (over)")
		map("<Leader>dsi", require("dap").step_into, "[D]ebugger Step [I]nto")
		map("<Leader>dso", require("dap").step_out, "[D]ebugger Step [O]ut")
		map("<Leader>db", require("dap").toggle_breakpoint, "[D]ebugger Toggle [b]reakpoint")
		map("<Leader>dB", require("dap").set_breakpoint, "[D]ebugger Set [B]reakpoint")
		map("<Leader>dl", function()
			require("dap").set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
		end, "[D]ebugger [L]ogger Breakpoint")
		map("<Leader>dor", require("dap").repl.open, "[D]ebugger [O]pen [R]epl")
		map("<Leader>dol", require("dap").repl.open, "[D]ebugger [O]pen [L]ast run")
		vim.keymap.set({ "n", "v" }, "<Leader>dh", require("dap.ui.widgets").hover, { desc = "[D]ebugger [H]over" })
		vim.keymap.set({ "n", "v" }, "<Leader>dp", require("dap.ui.widgets").preview, { desc = "[D]ebugger [P]review" })
		vim.keymap.set("n", "<Leader>dwf", function()
			local widgets = require("dap.ui.widgets")
			widgets.centered_float(widgets.frames)
		end, { desc = "[D]ebugger [W]idget [F]rames" })
		vim.keymap.set("n", "<Leader>dws", function()
			local widgets = require("dap.ui.widgets")
			widgets.centered_float(widgets.scopes)
		end, { desc = "[D]ebugger [W]idget [S]cope" })
	end,
}
