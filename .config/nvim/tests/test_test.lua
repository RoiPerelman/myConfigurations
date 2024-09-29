local T = MiniTest.new_set()

T['works'] = function()
  local x = 1 + 1
  print('ROIROI')
  if x ~= 2 then
    error('`x` is not equal to 2')
  end
end

T['works2'] = function()
  local x = 2 + 2
  print('ROIROI2')
  if x ~= 4 then
    error('`x` is not equal to 4')
  end
end
return T
