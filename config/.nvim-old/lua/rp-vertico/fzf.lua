local fzf = require "fzf_lib"

local M = {}

M.cache = {}
M.matches = {}

M.fzf_filter_sort = function(cache)
  local query = table.concat(cache.query, '')

  if M.cache[query] then
    cache.indices = vim.tbl_extend("force", {}, M.cache[query])
    M.matches[query] = vim.tbl_extend("force", {}, cache.matches)
    return
  end

  local slab = fzf.allocate_slab()
  local query_struct = fzf.parse_pattern(query)

  local filtered_score = {}
  for i, item in pairs(cache.items) do
    local score = fzf.get_score(item.text, query_struct, slab)
    -- Filter based on score
    if score > 0 then
      table.insert(filtered_score, { item = item, idx = i, score = score }) -- Add to filtered items
    end
  end

  -- Sort the filtered items by score (ascending order)
  table.sort(filtered_score, function(a, b)
    return a.score > b.score -- Sort by score
  end)

  -- Set filtered items in cache
  cache.indices = vim.tbl_map(function(entry) return entry.idx end, filtered_score)
  M.cache[query] = vim.tbl_extend("force", {}, cache.indices)

  cache.matches = vim.tbl_map(function(entry)
    return fzf.get_pos(entry.item.text, query_struct, slab)
  end, filtered_score)
  M.matches[query] = vim.tbl_extend("force", {}, cache.matches)

  -- Free resources
  fzf.free_pattern(query_struct)
  fzf.free_slab(slab)
end

return M
