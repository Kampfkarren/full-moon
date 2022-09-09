local ast

ast = parse("x, y = 1, 2")
error(tostring(ast))
