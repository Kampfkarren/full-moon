declare extern type Foo with
    function foo(self, x: number)
    @deprecated function bar(self)
    ["baz"]: string
    [string]: any
end