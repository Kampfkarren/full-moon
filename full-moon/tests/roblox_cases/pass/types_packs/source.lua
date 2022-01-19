function foo<T...>(param: () -> T...)
end

type Foo<T...> = () -> T...

function bar<T...>(...: T...)
end