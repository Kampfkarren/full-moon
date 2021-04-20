-- Not allowed tuples or `...type` in params
function foo(test: (number, number))
end

function bar(test: ...number)
end