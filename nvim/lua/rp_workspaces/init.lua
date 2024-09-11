local finders = require("telescope.finders")
local pickers = require("telescope.pickers")
local conf = require("telescope.config").values

M = {}

M.workspace = {
  "~/inspekto/tinybox",
  "~/inspekto/inspekto-os",
  "~/inspekto/meta-inspekto",
  "~/inspekto/fixi",
  "~/inspekto/fixi_client",
  "~/inspekto/tiny_database",
  "~/inspekto/tier2/tiny_std",
  "~/inspekto/tier2/common_std",
  "~/inspekto/data_coordinator",
  "~/inspekto/connectivity",
  "~/inspekto/inspekto_agent",
  "~/inspekto/integration_managers",
  "~/inspekto/agc",
  "~/inspekto/profile_center",
  "~/inspekto/inspekto_agent",
}

-- our picker function: colors
local workspace = function(opts)
  opts = opts or {}
  -- opts = require('telescope.themes').get_ivy{
  --   -- previewer = false,
  --   layout_config = {
  --     height = 13,
  --   },
  -- }
  pickers
    .new(opts, {
      prompt_title = "colors",
      finder = finders.new_table({
        results = M.workspace,
      }),
      sorter = conf.generic_sorter(opts),
    })
    :find()
end

-- to execute the function
workspace()
