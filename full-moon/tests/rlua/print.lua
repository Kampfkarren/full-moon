return function(fullMoon)
	local code = "local x = 1 -- code"
	local source = fullMoon.parse(code)
	local printed = source:print()
	if code ~= printed then
		error("print didn't return parsed code: " .. printed)
	end
end
