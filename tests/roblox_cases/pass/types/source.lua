type Identity<T> = T
type Array<T> = { [string]: number }
type Object = { x: number, y: number }
type Typeof = typeof(2 + 2 + foo())

type Callback1 = (string) -> number
type Callback2 = (string, string) -> number
type Callback3 = (string, string) -> (string, string)
type Callback4 = (string) -> (string) -> ()

local foo: number = 3
local foo: number?
local foo: Array<T>
local foo: Array<T, U>
local bar = foo as number
local foo: string, bar: string

local union: number | string
local multiUnion: number | string | nil

function foo(param: string): string
	return param
end

function foo(a: string, b: string, ...)
end

local foo = function(): number | nil
	return 3
end
