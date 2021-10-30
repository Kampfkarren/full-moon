local x = if foo then foo.x else 5
local y = (if x then x.indices else create()):update(if shouldUpdate then information else defaults)
local y = (if bar then foo.y else 5) :: number
