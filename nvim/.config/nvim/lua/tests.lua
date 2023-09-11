local filename = function()
  local str = debug.getinfo(2, "S").source:sub(2)
  return str:match("^.*/(.*).lua$") or str
end
print(filename())
