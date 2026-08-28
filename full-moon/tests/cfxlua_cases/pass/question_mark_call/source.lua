local fn = function(x, y)
  return x + y
end

local a = {
  cb = function(z)
    return z * 2
  end
}

local x = fn?(1, 2)
local y = a?.cb?(10)
