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

-- Test non-AST visiting
local numbers = {}

stmt:visit({
	Number = function(token)
		table.insert(numbers, token:print())
	end,
})

assertEq(#numbers, 2)
assertEq(numbers[1], "1")
assertEq(numbers[2], "2")

assert(not pcall(function()
	return stmt:expect("While")
end), "stmt:expect should have thrown")

local assignment = stmt:expect("Assignment")
assertEq(#assignment.variables, 2)
assertEq(#assignment.variables:values(), 2)

assertEq(tostring(stmt:match("Assignment")), tostring(assignment))
assertEq(stmt:match("While"), nil)

local iters = {}

for i, v in assignment.variables do
	iters[i] = v:print()
end

assertEq(#iters, 2)
assertEq(iters[1], "x")
assertEq(iters[2], "y ")
