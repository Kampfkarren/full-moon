while local line = reader:nextLine() do
	process(line)
end

while const item = queue:pop() do
	print(item)
end

while local x: number? = getValue() do
	if x > 10 then
		break
	end
	continue
end

while const name: string? = getName() do
	print(name)
end

local const = true
while const do
	const = false
end
