return function(fullMoon)
	local ast = fullMoon.parse([[
		local function returnsOne()
			return 1
		end

		local function returnsTwo()
			return 2
		end
	]])

	local called = {}

	local success = pcall(function()
		ast:visit({
			Doge = print,
		})
	end)

	assert(not success, "visit didn't mind the invalid node name")

	ast:visit({
		Return = function(node)
			table.insert(called, node:print():match("([0-9]+)"))
		end,
	})

	assert(called[1] == "1", "called[1] == " .. tostring(called[1]))
	assert(called[2] == "2", "called[2] == " .. tostring(called[2]))

	local message
	success, message = pcall(function()
		ast:visit({
			Return = function()
				error("oh noooo")
			end
		})
	end)

	assert(not success, "visit didn't error")
	assert(tostring(message):match("oh noooo"), "error didn't include the right info: " .. tostring(message))
end
