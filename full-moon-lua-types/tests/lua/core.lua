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

stmt:visit({
	Assignment = function(assignment)
		table.insert(assignments, assignment:print())
	end,
})

assertEq(#assignments, 1)
assertEq(assignments[1], "x, y = 1, 2")
