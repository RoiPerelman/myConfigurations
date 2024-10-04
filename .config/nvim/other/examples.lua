-- simple example input
vim.ui.input({ prompt = "Enter your name: " }, function(input)
  if input then
    print("Hello, " .. input .. "!")
  else
    print("You canceled the input.")
  end
end)

-- simple example select
local items = { "Option 1", "Option 2", "Option 3" }

vim.ui.select(items, {
  prompt = "Choose an option:",
  format_item = function(item)
    return "-> " .. item
  end,
}, function(choice, idx)
  if choice then
    print("You chose: " .. choice .. " (Index: " .. idx .. ")")
  else
    print("You canceled")
  end
end)
