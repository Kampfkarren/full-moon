type MyCallbackType = (cost: number, name: string) -> string

local cb: (amount: number) -> number
local function foo(cb: (name: string) -> ())
end