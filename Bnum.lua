--!strict
export type BN = {man: number, exp: number}
export type Bnum = {
	new: (man: number, exp: number) -> BN,
	isFinite: (val: BN) -> boolean,
	fromNumber: (val: number) -> BN,
	toNumber: (val: BN) -> number,
	fromString: (str: string) -> BN,
	toString: (val: BN) -> string,
	convert: (val: any) -> BN,
	add: (val1: any, val2: any) -> BN,
	neg: (val: any) -> BN,
	sub: (val1: any, val2: any) -> BN,
	mul: (val1: any, val2: any) -> BN,
	recip: (val: any) -> BN,
	div: (val1: any, val2: any) -> BN,
	pow: (val1: any, val2: any) -> BN,
	sqrt: (val: any) -> BN,
	cbrt: (val: any) -> BN,
	root: (val1: any, val2: any) -> BN,
	pow10: (val: any) -> BN,
	log: (val1: any, val2: any) -> BN,
	logn: (val: any) -> BN,
	log10: (val: any) -> BN,
	compare: (val1: any, val2: any) -> number,
	le: (val1: any, val2: any) -> boolean,
	leeq: (val1: any, val2: any) -> boolean,
	me: (val1: any, val2: any) -> boolean,
	meeq: (val1: any, val2: any) -> boolean,
	eq: (val1: any, val2: any) -> boolean,
	between: (val: any, lower: any, upper: any, inclusive: boolean?) -> boolean,
	suffixPart: (index: number) -> string,
	short: (val: any) -> string,
	toScienctific: (val: any) -> string,
	toHyperE: (val: any) -> string,
	shortE: (val: any) -> string,
	format: (val: any) -> string,
	min: <T...>(T...) -> BN,
	max: <T...>(T...) -> BN,
	clamp: (val: any, min: any, max: any) -> BN,
	floor: (val: any) -> BN,
	ceil: (val: any) -> BN,
	round: (val: any) -> BN,
	exp: (val: any) -> BN,
	mod: (val1: any, val2: any) -> BN,
	modf: (val: any) -> (BN, BN),
	fmod: (val1: any, val2: any) -> BN,
	pow2: (val: any) -> BN
}
local Bn = {}
local inf = math.huge
local neginf = -inf
local nan = 0/0
local zero: BN = {man=0, exp=0}
local one: BN = {man=1, exp=0}
local first = {'', 'k', 'm', 'b'}
local firstset = {"", "U","D","T","Qd","Qn","Sx","Sp","Oc","No"}
local second = {"", "De","Vt","Tg","qg","Qg","sg","Sg","Og","Ng"}
local third = {"", "Ce", "Du","Tr","Qa","Qi","Se","Si","Ot","Ni"}

function Bn.new(man: number, exp: number): BN
	return {man=man,exp=exp}
end

function Bn.isFinite(val: BN): boolean
	return val.exp < 308 and val.exp > -308
end

function Bn.fromNumber(val: number): BN
	if val == 0 then return zero end
	if val == inf then return {man = 1, exp = inf} end
	if val == neginf then return {man=1, exp = inf} end
	if val ~= val then return {man=0, exp = nan} end
	local absVal = math.abs(val)
	local exp = math.floor(math.log10(absVal))
	local man = val/10^exp
	return Bn.new(man, exp)
end

function Bn.toNumber(val: BN): number
	local man, exp = val.man, val.exp
	if exp == inf then
		return val.man >= 0 and inf or neginf
	elseif exp ~= exp then
		return nan
	elseif man == 0 then
		return 0
	end
	return man * 10^exp
end

function Bn.fromString(str: string): BN
	if str:find('e') then
		local base, expStr = str:match('^([+-]?%d*%.?%d+)[eE](.+)$')
		if base and expStr then
			local man = tonumber(base):: number
			local expBn = Bn.fromString(expStr)
			local exp = Bn.toNumber(expBn)
			return Bn.new(man, exp)
		end
	end
	local val = tonumber(str)
	if val then
		return Bn.fromNumber(val)
	end
	return {man = 0, exp = nan}
end

function Bn.toString(val: BN): string
	local man, exp = val.man, val.exp
	if exp ~= exp then return 'NaN' end
	if exp == inf then
		return man >= 0 and 'Inf' or '-Inf'
	end
	if man == 0 then return '0' end
	if exp <= 308 then
		if exp == 0 then	return tostring(man) end
		return man .. 'e' .. exp
	end
	local hyperE = exp
	local hyperParts: {[number]: number} = {}
	while hyperE > 308 do
		local layer = math.floor(math.log10(hyperE))
		if layer >= 308 then
			layer = inf
		end
		table.insert(hyperParts, layer)
		hyperE = hyperE / 10^layer
	end
	table.insert(hyperParts, hyperE)
	local str = tostring(man)
	for i, part in ipairs(hyperParts) do
		str = str .. 'e' .. part
	end
	return str
end

function Bn.convert(val: any): BN
	if typeof(val) == 'number' then
		return Bn.fromNumber(val)
	elseif typeof(val) == 'string' then
		return Bn.fromString(val)
	elseif typeof(val) == 'table' then
		local BN: {number} = val
		if not val.man or not val.exp then
			return {man = BN[1], exp = BN[2]}
		end
		local val2: BN = val
		return {man = val2.man, exp = val2.exp}
	end
	error('Failed to convert to PrettyNum')
	return {man = 0, exp = 0}
end

function Bn.add(val1: any, val2: any): BN
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	local man1: number, man2: number = val1.man, val2.man
	local exp1: number, exp2: number = val1.exp, val2.exp
	if exp1 == inf or exp2 == inf then
		if (exp1 == inf and man1>0) or (exp2 == inf and man2 > 0) then
			return {man=1, exp = inf}
		end
		return {man=-1,exp=inf}
	end
	if exp1~= exp1 or exp2 ~= exp2 then return {man=0, exp = nan} end
	if man1 == 0 then return val2 end
	if man2 == 0 then return val1 end
	if exp1 > exp2 +15 then
		return val1
	elseif exp2 > exp1 + 15 then
		return val2
	end
	local diff = exp1 - exp2
	local sumMan: number, sumExp: number
	if diff >= 0 then
		sumMan = man1+man2 / 10^diff
		sumExp = exp1
	else
		sumMan = man1/10^-diff + man2
		sumExp = exp2
	end
	if sumMan >= 10 then
		local shift = math.floor(math.log10(sumMan:: number))
		sumMan = sumMan / 10^shift
		sumExp = sumExp + shift
	end
	return {man = sumMan, exp = sumExp}
end

function Bn.neg(val: any): BN
	val = Bn.convert(val)
	return {man = -val.man, exp = val.exp}
end

function Bn.sub(val1: any, val2: any): BN
	return Bn.add(val1, Bn.neg(val2))
end

function Bn.mul(val1: any, val2: any): BN
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	local man1: number, man2: number = val1.man, val2.man
	local exp1: number, exp2: number = val1.exp, val2.exp
	if man1 == 0 or man2 == 0 then return {man=0, exp=0} end
	if exp1 == inf or exp2 == inf then
		local sign = (man1 * man2 >= 0) and 1 or -1
		return {man=sign, exp=inf}
	end
	if exp1 ~= exp1 or exp2 ~= exp2 then return {man=0, exp=0/0} end
	local newMan: number = man1 * man2
	local newExp: number = exp1+exp2
	if newMan >= 10 then
		local shift = math.floor(math.log10(newMan))
		newMan = newMan / 10^shift
		newExp = newExp + shift
	elseif newMan < 1 then
		local shift = math.floor(math.log10(newMan))
		newMan = newMan/10^shift
		newExp = newExp + shift
	end
	return {man = newMan, exp = newExp}
end

function Bn.recip(val: any): BN
	local v = Bn.convert(val)
	if v.man == 0 then
		return {man = 1, exp = inf}
	end
	return {man = 1/v.man, exp = -v.exp}
end

function Bn.div(val1: any, val2: any): BN
	return Bn.mul(val1, Bn.recip(val2))
end

function Bn.pow(val1: any, val2: any): BN
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1.man == 0 then
		if val2.man == 0 then return {man=1, exp = 0} end
		return {man = 0, exp = 0}
	end
	if val1.exp == inf then return {man=1, exp=inf} end
	if val2.exp == inf then return {man=1, exp=inf} end
	if val1.exp ~= val1.exp or val2.exp ~= val2.exp then return {man=0, exp=neginf} end
	local logVal1: number = math.log10(val1.man) + val1.exp
	local L: number = logVal1 * (val2.man:: number * 10^val2.exp:: number)
	local newExp: number = math.floor(L)
	local newMan: number = 10^(L - newExp)
	if newMan >= 10 then
		local shift = math.floor(math.log10(newMan))
		newMan = newMan / 10^shift
		newExp = newExp + shift
	elseif newMan < 1 then
		local shift = math.floor(math.log10(newMan))
		newMan = newMan / 10^shift
		newExp = newExp + shift
	end
	return {man = newMan, exp = newExp}
end

function Bn.sqrt(val: any): BN
	val = Bn.convert(val)
	if val.man < 0 then return {man=0,exp=nan} end
	local exp = val.exp/2
	local man = val.man ^0.5
	if man >= 10 then
		local shift = math.floor(math.log10(man))
		man = man / 10^shift
		exp = exp + shift
	elseif man < 1 then
		local shift = math.floor(math.log10(man))
		man = man / 10^shift
		exp = exp + shift
	end
	return {man=man, exp=exp}
end

function Bn.cbrt(val: any): BN
	val = Bn.convert(val)
	if val.man < 0 then return {man=0,exp=nan} end
	local exp = val.exp/3
	local man = val.man ^ (1/3)
	if man >= 10 then
		local shift = math.floor(math.log10(man))
		man = man / 10^shift
		exp = exp + shift
	elseif man < 1 then
		local shift = math.floor(math.log10(man))
		man = man / 10^shift
		exp = exp + shift
	end
	return {man=man, exp=exp}
end

function Bn.root(val1: any, val2: any): BN
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1.man < 0 then
		if val2.man % 2 == 0 and val2.exp == 0 then
			return {man = 0, exp = nan}
		end
	end
	local recip = Bn.recip(val2)
	return Bn.pow(val1, recip)
end

function Bn.pow10(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 then return {man = 1, exp = 0} end
	if val.exp == inf then return {man=1, exp = inf} end
	if val.exp ~= val.exp then return {man = 0, exp = nan} end
	local newExp = val.man * 10^val.exp
	local newMan: number = 1
	if newMan >= 10 then
		local shift = math.floor(math.log10(newMan))
		newMan = newMan / 10^shift
		newExp = newExp + shift
	end
	return {man = newMan, exp = newExp}
end

function Bn.log(val1: any, val2: any): BN
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1.man <= 0 or val2.man <= 0 then
		return {man = 0, exp = neginf}
	end
	if val1.exp == inf then return {man = 1, exp = inf} end
	if val2.exp == inf then return {man = 0, exp = 0} end
	local ln10: number = math.log10(10)
	local ln_val1: number = math.log(val1.man) + val1.exp * ln10
	local ln_val2: number = math.log(val2.man:: number) + val2.exp:: number * ln10
	local log_val: number = ln_val1/ln_val2
	if log_val == 0 then
		return {man = 0, exp = 0}
	elseif math.abs(log_val) < 10 then
		return {man = log_val, exp = 0}
	end
	local expPart: number = math.floor(math.log10(math.abs(log_val)))
	local manPart: number = log_val / (10^expPart)
	return {man = manPart, exp = expPart}
end

function Bn.logn(val: any): BN
	val = Bn.convert(val)
	if val.man <= 0 then
		return {man = 0, exp = neginf}
	end
	if val.exp == inf then
		return {man = 1, exp = inf}
	end
	if val.man == 1 and val.exp == 0 then
		return {man = 0, exp = 0}
	end
	local ln10 = math.log(10)
	local result = math.log(val.man) + val.exp * ln10
	if result == 0 then
		return {man = 0, exp = 0}
	elseif math.abs(result) < 10 then
		return {man = result, exp = 0}
	end
	local expPart = math.floor(math.log10(math.abs(result)))
	local manPart = result / (10^expPart)
	return {man = manPart, exp = expPart}
end

function Bn.log10(val: any): BN
	val = Bn.convert(val)
	if val.man <= 0 then
		return {man = 0, exp = neginf}
	end
	if val.exp == inf then
		return {man = 1, exp = inf}
	end
	local result = math.log10(val.man) + val.exp
	if math.abs(result) < 10 then
		return {man=result, exp = 0}
	end
	local exp = math.floor(math.log10(math.abs(result)))
	local man = result / (10^exp)
	return {man = man, exp = exp}
end

function Bn.compare(val1: any, val2: any): number
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1.exp ~= val1.exp or val2.exp ~= val2.exp then
		return 0
	end
	if val1.exp == inf or val2.exp == inf then
		if val1.exp == inf and val2.exp == inf then
			if val1.man == val2.man then return 0 end
			return val1.man > val2.man:: number and 1 or -1
		elseif val1.exp == inf then
			return val1.man > 0 and 1 or -1
		else
			return val2.man > 0 and -1 or 1
		end
	end
	if val1.man == 0 and val2.man == 0 then return 0 end
	if val1.exp ~= val2.exp then
		return val1.exp > val2.exp:: number and 1 or -1
	end
	if math.abs(val1.man - val2.man) < 1e-12 then
		return 0
	end
	return val1.man > val2.man and 1 or -1
end

function Bn.le(val1: any, val2: any): boolean
	return Bn.compare(val1, val2) < 0
end

function Bn.leeq(val1: any, val2: any): boolean
	local cmp = Bn.compare(val1, val2)
	return cmp < 0 or cmp == 0
end

function Bn.me(val1: any, val2: any): boolean
	return Bn.compare(val1, val2) > 0
end

function Bn.meeq(val1: any, val2: any): boolean
	local cmp = Bn.compare(val1, val2)
	return cmp > 0 or cmp == 0
end

function Bn.eq(val1: any, val2: any): boolean
	return Bn.compare(val1, val2) == 0
end

function Bn.between(val: any, lower: any, upper: any, inclusive: boolean?): boolean
	val, lower, upper = Bn.convert(val), Bn.convert(lower), Bn.convert(upper)
	inclusive = inclusive ~= false
	if inclusive then
		return Bn.compare(val, lower) >= 0 and Bn.compare(val, upper) <= 0
	end
	return Bn.compare(val, lower) > 0 and Bn.compare(val, upper) < 0
end

function Bn.suffixPart(index: number): string
	local hund = math.floor(index/100)
	index = math.fmod(index, 100)
	local ten = math.floor(index/10)
	index = math.fmod(index, 10)
	local one = math.floor(index/1)
	return (firstset[one+1] or '') .. (second[ten+1] or '') .. (third[hund+1] or '')
end

function Bn.short(val: any): string
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	local lf = exp % 3
	man = math.floor((man*10^lf) * 100) / 100
	if exp ~= exp then return 'NaN' end
	if exp == inf then return man >= 0 and 'Inf' or '-Inf' end
	if man == 0 then return '0' end
	if exp < 0 then
		local index = math.floor(-exp / 3)
		if index <= #first then
			return '1/' ..man.. first[index + 1]
		end
		return '1/' ..man.. Bn.suffixPart(index-1)
	end
	if exp < 3 then
		local num = Bn.toNumber(val)
		return tostring(math.floor(num + 0.001))
	end
	local index = math.floor(exp/3)
	if index < #first then
		return man .. first[index+1] or ''
	end
	local suffix = index - 1
	local scaled = man / 10^(suffix*3)
	return man .. Bn.suffixPart(suffix)
end

function Bn.toScienctific(val: any): string
	val = Bn.convert(val)
	return val.man .. 'e' .. val.exp
end

function Bn.toHyperE(val: any): string
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	if exp ~= exp then return "NaN" end
	if exp == inf then return man >= 0 and "Inf" or "-Inf" end
	if man == 0 then return "0" end
	if exp <= 308 then
		return Bn.toScienctific(val)
	end
	local function hyperE(e: number): string
		if e <= 308 then
			return tostring(e)
		end
		local top = math.floor(math.log10(e))
		local rest = e / 10^top
		return man .. 'e' .. hyperE(top)
	end
	return man .. 'e' .. hyperE(exp)
end

function Bn.shortE(val: any): string
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	if exp ~= exp then return "NaN" end
	if exp == inf then return man >= 0 and "Inf" or "-Inf" end
	if man == 0 then return "0" end
	if exp < 1000 then
		return man .. 'e' .. exp
	end
	local expBn = Bn.fromNumber(exp)
	local expStr = Bn.short(expBn)
	return 'E' .. expStr
end

function Bn.format(val: any): string
	if Bn.meeq(val, '1e1e60') then
		return Bn.toHyperE(val)
	elseif Bn.meeq(val, '1e1e30') then
		return Bn.shortE(val)
	end
	return Bn.short(val)
end

function Bn.min<T...>(...: T...): BN
	local args = {}
	if #args == 0 then return zero end
	local best = Bn.convert(args[1])
	for i = 2, #args do
		local val = Bn.convert(args[i])
		if Bn.compare(val, best) < 0 then
			best = val
		end
	end
	return best
end

function Bn.max<T...>(...: T...): BN
	local args = {}
	if #args == 0 then return zero end
	local best = Bn.convert(args[1])
	for i = 2, #args do
		local val = Bn.convert(args[i])
		if Bn.compare(val, best) > 0 then
			best = val
		end
	end
	return best
end

function Bn.clamp(val: any, min: any, max: any): BN
	if Bn.compare(min, max) > 0 then
		min, max = max, min
	end
	if Bn.compare(val, min) < 0 then
		return min
	elseif Bn.compare(val, max) > 0 then
		return max
	end
	return val
end

function Bn.floor(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 and val.exp  == inf or val.exp ~= val.exp then
		return val
	end
	local man, exp = val.man, val.exp
	if exp > 0 then
		man = math.floor(man)
		return {man=man,exp=exp}
	end
	local sMan = man * 10^exp
	local floor = math.floor(sMan)
	if floor == 0 then return zero end
	local nExp = math.floor(math.log10(floor))
	local nMan = floor / 10^nExp
	return {man = nMan, exp = nExp}
end

function Bn.ceil(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 and val.exp  == inf or val.exp ~= val.exp then
		return val
	end
	local man, exp = val.man, val.exp
	if exp > 0 then
		man = math.ceil(man)
		return {man =man, exp = exp}
	end
	local sMan = man * 10^exp
	local ceil = math.ceil(sMan)
	if ceil == 0 then return zero end
	local nExp = math.floor(math.log10(ceil))
	local nMan = ceil/10^nExp
	return {man=nMan, exp = nExp}
end

function Bn.round(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 and val.exp  == inf or val.exp ~= val.exp then
		return val
	end
	local man, exp = val.man, val.exp
	if exp > 0 then
		man = math.round(man + 0.001)
		return {man = man, exp = exp}
	end
	local sMan = man * 10^exp
	local round = math.floor(sMan + 0.001)
	if round == 0 then return zero end
	local nExp = math.floor(math.log10(round))
	local nMan = round / 10^nExp
	return {man = nMan, exp = nExp}
end

function Bn.exp(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 then return one end
	if val.exp == inf then return {man=1, exp=inf} end
	if val.exp ~= val.exp then return {man=1, exp = nan} end
	local l10e = Bn.fromNumber(0.4342944819032518)
	local pow = Bn.mul(val, l10e)
	return Bn.pow10(pow)
end

function Bn.mod(val1: any, val2: any): BN
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val2.man == 0 then return {man=1, exp = nan} end
	local cmp = Bn.compare(val1, val2)
	if cmp < 0 then
		return val1
	elseif cmp == 0 then
		return zero
	end
	local ratio = Bn.div(val1, val2)
	local floor = Bn.floor(ratio)
	local prod = Bn.mul(floor, val2)
	return Bn.sub(val1, prod)
end

function Bn.modf(val: any): (BN, BN)
	val = Bn.convert(val)
	if val.man == 0 and val.exp == inf or val.exp ~= val.exp then
		return val, zero
	end
	local int = Bn.floor(val)
	local frac = Bn.sub(val, int)
	return int ,frac
end

function Bn.fmod(val1: any, val2: any): BN
	local q = Bn.div(val1, val2)
	local qF = Bn.floor(q)
	local prod = Bn.mul(qF, val2)
	return Bn.sub(val1, prod)
end

function Bn.pow2(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 then return one end
	if val.exp == inf then return {man=1, exp = inf} end
	local l2 = 0.3010299956639812
	local nExp = val.man * 10^val.exp * l2
	local man = 1
	if nExp >= 10 then
		local shift = math.floor(math.log10(nExp))
		nExp = nExp
	end
	return {man = man, exp = nExp}
end

return Bn:: Bnum