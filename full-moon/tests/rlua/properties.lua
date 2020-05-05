return function(fullMoon)
	local astReturnNil = fullMoon.parse("return nil")
	assert(astReturnNil.nodes.lastStmt ~= nil, "lastStmt was nil in `return nil`")

	local astNoReturn = fullMoon.parse("local x = 1")
	assert(astNoReturn.nodes.lastStmt == nil, "lastStmt was not nil in ast without it")
end
