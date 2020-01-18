```
chunk ::= {stat [`;´]} [laststat [`;´]]

block ::= chunk

stat ::=  varlist `=´ explist |
	functioncall |
	do block end |
	while exp do block end |
	repeat block until exp |
	if exp then block {elseif exp then block} [else block] end |
	for Name `=´ exp `,´ exp [`,´ exp] do block end |
	for namelist in explist do block end |
	function funcname funcbody |
	local function Name funcbody |
	local namelist [`=´ explist]

laststat ::= return [explist] | break

funcname ::= Name {`.´ Name} [`:´ Name]

varlist ::= var {`,´ var}

namelist ::= Name {`,´ Name}

explist ::= {exp `,´} exp

prefixexp ::= var | functioncall | `(´ exp `)´

args ::=  `(´ [explist] `)´ | tableconstructor | String

function ::= function funcbody

funcbody ::= `(´ [parlist] `)´ block end

parlist ::= namelist [`,´ `...´] | `...´

tableconstructor ::= `{´ [fieldlist] `}´

fieldlist ::= field {fieldsep field} [fieldsep]

field ::= `[´ exp `]´ `=´ exp | Name `=´ exp | exp

fieldsep ::= `,´ | `;´

binop ::= `+´ | `-´ | `*´ | `/´ | `^´ | `%´ | `..´ |
	 `<´ | `<=´ | `>´ | `>=´ | `==´ | `~=´ |
	 and | or

unop ::= `-´ | not | `#´
```

Modified rules from http://lua-users.org/lists/lua-l/2010-12/msg00699.html
```
value ::= nil | false | true | Number | String | '...' | function |
	tableconstructor | functioncall | var | '(' exp ')'
exp ::= unop exp | value [binop exp]
prefix ::= '(' exp ')' | Name
index ::= '[' exp ']' | '.' Name
call ::= args | ':' Name args
suffix ::= call | index
var ::= prefix {suffix} index | Name
functioncall ::= prefix {suffix} call
```