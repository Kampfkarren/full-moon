function foo<x, y>()
end

local function bar<x>()
end

export type Foo = {
	bar: <T>(
		a: T,
		b: nil | number | boolean
	) -> T,
}
