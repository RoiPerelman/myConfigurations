vim.opt.foldmethod = "expr"
vim.opt.foldexpr   = "v:lua.lua_dash_sections_foldexpr(v:lnum)"
vim.opt.foldenable = true
vim.opt.foldlevel  = 99 -- Show all unfolded by default
-- vim.opt.foldcolumn = "4" -- Show fold column with levels

-- Custom fold text that shows just the header content and line count
vim.opt.foldtext   = "v:lua.lua_dash_foldtext()"

local function header_level(line)
  -- Match: start of line, optional spaces, then 3+ dashes, then optional space/text
  local dashes = line:match("^%s*(%-%-%-+)%s*.*$")
  if dashes and #dashes >= 3 then
    return #dashes - 2 -- '---' => 1, '----' => 2, etc.
  end
  return nil
end

-- Custom fold text function
function _G.lua_dash_foldtext()
  local foldstart = vim.v.foldstart
  local foldend = vim.v.foldend
  local line = vim.fn.getline(foldstart)

  -- Extract header text (everything after the dashes)
  local header_text = line:match("^%s*%-%-%-+%s*(.*)$")
  if header_text and header_text ~= "" then
    -- Clean up the header text
    header_text = vim.trim(header_text)
  else
    header_text = "Header"
  end

  local line_count = foldend - foldstart + 1
  return header_text .. " (" .. line_count .. " lines)"
end

-- Fold expression for Lua dash-based headers
function _G.lua_dash_sections_foldexpr(lnum)
  local line = vim.fn.getline(lnum)

  -- If current line is a header, return its level prefixed with '>'
  local lvl = header_level(line)
  if lvl then
    return '>' .. lvl
  end

  -- Check next line to see if it's a header (for proper folding end)
  local next_line = vim.fn.getline(lnum + 1)
  local next_lvl = header_level(next_line)

  -- If next line is a header, check if we need to close current fold
  if next_lvl then
    -- Find the level of current content by looking at nearest header above
    local i = lnum - 1
    while i > 0 do
      local prev = vim.fn.getline(i)
      local plvl = header_level(prev)
      if plvl then
        -- If next header is same level or higher (lower number), close current fold
        if next_lvl <= plvl then
          return '<' .. plvl
        end
        break
      end
      i = i - 1
    end
  end

  -- Otherwise, inherit from the nearest header above
  local i = lnum - 1
  while i > 0 do
    local prev = vim.fn.getline(i)
    local plvl = header_level(prev)
    if plvl then
      return plvl
    end
    i = i - 1
  end

  -- No header above -> no folding
  return 0
end
