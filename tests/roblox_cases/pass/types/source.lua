type Identity<T> = T
type Array<T> = { [string]: number }
type Object = { x: number, y: number }

type Callback1 = (string) => number
type Callback2 = (string, string) => number
type Callback3 = (string, string) => (string, string)

local foo: number = 3
local foo: number?
local foo: Array<T>
local bar = foo as number

local union: number | string

function foo(param: string) => string
	return param
end

local foo = function() => number | nil
	return 3
end
