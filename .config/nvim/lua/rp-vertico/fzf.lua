local fzf = require "fzf_lib"

local M = {}

M.fzf_filter_sort = function(cache)
  local query = table.concat(cache.query, '')

  local slab = fzf.allocate_slab()
  local query_struct = fzf.parse_pattern(query)

  local filtered_score = {}
  for _, item in pairs(cache.items) do
    local score = fzf.get_score(item, query_struct, slab)
    -- Filter based on score
    if score > 0 then
      table.insert(filtered_score, { item = item, score = score }) -- Add to filtered items
    end
  end

  -- Sort the filtered items by score (ascending order)
  table.sort(filtered_score, function(a, b)
    return a.score < b.score -- Sort by score
  end)

  -- Set filtered items in cache
  cache.filtered_sorted_items = vim.tbl_map(function(entry) return entry.item end, filtered_score)

  -- Free resources
  fzf.free_pattern(query_struct)
  fzf.free_slab(slab)
end

return M
