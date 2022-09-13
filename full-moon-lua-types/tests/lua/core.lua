local function assertEq(x, y)
	if x == y then
		return
	end

	error(("%s ~= %s"):format(tostring(x), tostring(y)))
end

local ast = full_moon.parse("x, y = 1, 2")
assert(not pcall(function()
	ast.nodes = {}
end), "expected ast.nodes to be read-only")

local stmt = ast.nodes.stmts[1]

assertEq(tostring(stmt), "Stmt::Assignment")
assertEq(stmt.kind, "Assignment")

assertEq(
	stmt:match({
		Assignment = function(assignment)
			local saysAssignment = tostring(assignment):match("^[A-Za-z]+")

			return assert(saysAssignment, "tostring(assignment) didn't match (" .. tostring(assignment) .. ")")
		end,
	}),

	"Assignment"
)

local assignments = {}
local assignmentEnds = {}

ast:visit({
	Assignment = function(assignment)
		table.insert(assignments, assignment:print())
	end,

	AssignmentEnd = function(assignmentEnd)
		table.insert(assignmentEnds, assignmentEnd:print())
	end,
})

assert(not pcall(function()
	ast:visit({
		Funky = function() end,
	})
end), "expected :visit to not allow invalid names")

assert(not pcall(function()
	ast:visit({
		Assignment = 3,
	})
end), "expected :visit to not allow invalid values")

assertEq(#assignments, 1)
assertEq(assignments[1], "x, y = 1, 2")

assertEq(#assignmentEnds, 1)
assertEq(assignmentEnds[1], "x, y = 1, 2")
