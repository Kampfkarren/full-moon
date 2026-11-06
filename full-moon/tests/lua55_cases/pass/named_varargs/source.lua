local function collect(...args) return args.n, ... end
function ordinary(first, ...rest) return first, rest[1] end
local anonymous = function(...items) return items end
function object:method(...values) return values.n end
