-- fractional hexadecimal
local a = 0x0.1E

-- binary exponent in hexadecimal
local b = 0xA23p-4

-- a mixture of both
local c = 0X1.921FB54442D18P+1

-- LuaJIT numbers (ULL/LL ending for both decimal and hexidecimal, or imaginary)
-- This is in Lua 5.2 tests for simplicity
-- rewrite todo: add this back, but in a separate luajit pass folder
-- local d = 42LL
-- local e = 0x2aLL
-- local f = 12.5i
