--!strict
export type BN = {man: number, exp: number}
type HN = {man: number, exp: number, layer: number}

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

-- helps create a new BN
function Bn.new(man: number, exp: number): BN
	return {man=man,exp=exp}
end

function Bn.isFinite(val: BN): boolean
	return val.exp < 308 and val.exp > -308
end

-- converts lets say 1235235 converts to {man = 1.235235, exp = 5}
function Bn.fromNumber(val: number): BN
	if val == 0 then return zero end
	if val == inf then return {man = 1, exp = inf} end
	if val == neginf then return {man=-1, exp = inf} end
	if val ~= val then return {man=0, exp = nan} end
	local absVal = math.abs(val)
	local exp = math.floor(math.log10(absVal))
	local man = val/10^exp
	return Bn.new(man, exp)
end

-- helps convert {man = 1.2, exp = 1} to convert do 12
function Bn.toNumber(val: BN): number
	local man, exp = val.man, val.exp
	if exp == inf then
		return val.man >= 0 and inf or neginf
	elseif exp ~= exp then
		return 0
	elseif man == 0 then
		return 0
	end
	return man * 10^exp
end

-- a helper to convert '1e1' to {man = 1, exp = 1} or exp = Bn.fromString(exp) to help convert '1e1e1' back to {man = 1, exp = 10}
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

-- helps convert BN like {man = 1, exp = 1} to 1e1 or {man = 1, exp = 1e3} to 1e1e3
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

-- a helper func that helps convert string like 1e1e1 to 1e10, 100 to 1e2 or {1, 2} back to {man = man, exp = exp}
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

-- so diff(BN1.exp, BN2.exp) if diff >= 0 then BN1.man + (BN2.man/10^diff) but if its not then (BN1.man / 10^diff) + BN2.man
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

-- a helper if u want todo Bn.sub(1, 2) for Bn.add(val1, Bn.neg(val2))
function Bn.neg(val: any): BN
	val = Bn.convert(val)
	return {man = -val.man, exp = val.exp}
end

-- makes sure BN is positive 
function Bn.abs(val: any): BN
	val = Bn.convert(val)
	return Bn.new(math.abs(val.man), val.exp)
end

-- so diff(BN1.exp, BN2.exp) if diff >= 0 then BN1.man - (BN2.man/10^diff) but if its not then (BN1.man / 10^diff) - BN2.man
function Bn.sub(val1: any, val2: any): BN
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1.exp ~= val1.exp or val2.exp ~= val2.exp then return {man = 0, exp = nan} end
	if val1.exp == inf or val2.exp == inf then
		if val1.exp == inf and val2.exp == inf then
			if val1.man > val2.man then return {man = 1, exp = inf} end
			return zero
		end
		if val2.exp == inf then return zero end
		if val1.exp == inf then return {man = val1.man >= 0 and 1 or -1, exp = inf} end
	end
	if Bn.leeq(val1, val2) then return zero end
	local man1, man2 = val1.man, val2.man
	local exp1, exp2 = val1.exp, val2.exp
	if exp1 > exp2 + 15 then return val1 elseif exp2 > exp1 +15 then return zero end
	local diff = exp1 - exp2
	local resMan, resExp
	if diff >= 0 then
		resMan = man1 - (man2 / (10^diff))
		resExp = exp1
	else
		resMan = (man1 / (10^diff)) - man2
		resExp = exp2
	end
	if resMan <= 0 or (resMan < 0 and math.abs(resMan) < 1e-12) then
		return zero
	end
	if resMan >= 10 then
		local shift = math.floor(math.log10(resMan))
		resMan = resMan / (10 ^ shift)
		resExp = resExp + shift
	else
		if resMan < 1 then
			local shift = math.floor(math.log10(resMan))
			resMan = resMan / (10 ^ shift)
			resExp = resExp + shift
		end
	end
	return {man = resMan, exp = resExp}
end

-- mul is BN1.man * BN2.man, BN1.exp + BN2.exp so 1e2 * 1.5e1 is {man=3.5, exp = 3}
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

-- makes sure that BN converts so mul can do div
function Bn.recip(val: any): BN
	local v = Bn.convert(val)
	if v.man == 0 then
		return {man = 1, exp = inf}
	end
	return {man = 1/v.man, exp = -v.exp}
end

-- acts like mul but reverse since of recip
function Bn.div(val1: any, val2: any): BN
	return Bn.mul(val1, Bn.recip(val2))
end

-- just a regular pow so 10^3 = 1000 cant do what pow10 does
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

--sqrt is BN.exp / 2, BN.man ^ 0.5
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

-- cbrt is BN.man ^ (1/3)
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

-- so root is pow(val1, recip(val2))
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

-- so pow10 is BN.man * 10^exp so 1e2 = 1e20
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

-- so log(val1, val2) = log(BN1.man) + BN1.exp * log10(10) / log(BN2.man) + BN2.exp * log10(10)
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

-- takes BN then log10(BN.man) + BN.exp * log(10)
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

-- takes val = math.log10(BN.man) + BN.exp
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

-- converts BN so its able to make calc for like if Bn.meeq(val1, val2) then like for Incremental games
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

--makes sure BN val1, val2 < 0 so val1 < val2 like 1e3 < 1e5 = true 1e2 < 1e1 = false
function Bn.le(val1: any, val2: any): boolean
	return Bn.compare(val1, val2) < 0
end

-- makes sure BN val1, val2 < 0 or val1 == val2 makes 1e2 <= 1e1 = false, 1e1 >= 1e3 = true
function Bn.leeq(val1: any, val2: any): boolean
	local cmp = Bn.compare(val1, val2)
	return cmp < 0 or cmp == 0
end

--makes sure BN val1, val2 > 0 so val1 > val2 like 1e3 > 1e5 = false 1e2 > 1e1 = true
function Bn.me(val1: any, val2: any): boolean
	return Bn.compare(val1, val2) > 0
end

-- makes sure BN val1, val2 > 0 or val1 == val2 makes 1e2 >= 1e1 = true, 1e1 >= 1e3 = false
function Bn.meeq(val1: any, val2: any): boolean
	local cmp = Bn.compare(val1, val2)
	return cmp > 0 or cmp == 0
end

-- makes sure BN val1, val2 == 0 so 1e3 == 1e2 = false, 1e1 == 1e1 = true
function Bn.eq(val1: any, val2: any): boolean
	return Bn.compare(val1, val2) == 0
end

--ex 1 < 5 > 10 for between
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

-- acts so u can do 1k for 1e3, 1e30 -No and so on
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
		return tostring(math.floor(num * 100 + 0.001) / 100)
	end
	local index = math.floor(exp/3)
	if index < #first then
		return man .. first[index+1] or ''
	end
	local suffix = index - 1
	local scaled = man / 10^(suffix*3)
	return man .. Bn.suffixPart(suffix)
end

-- just like toString but instead its just man .. 'e' .. exp so 1.23e1308 doesnt convert toHyperE
function Bn.toScienctific(val: any): string
	val = Bn.convert(val)
	return val.man .. 'e' .. val.exp
end

-- converts 1e1000 down to 1e1e3
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

-- acts like short but on a different note as in grabs exp instead so like 1e2 is just 1e2 after 1e1000 its E1k up to E100UCe
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

--formats short(1e3) to 1k, shortE(1e1e3) acts as E1k and toHyperE(1e1e61) is just 1e1e61
function Bn.format(val: any): string
	if Bn.meeq(val, '1e1e60') then
		return Bn.toHyperE(val)
	elseif Bn.meeq(val, '1e1e30') then
		return Bn.shortE(val)
	end
	return Bn.short(val)
end

-- gets the lowest like 1, 1.5e10 only grabs 1
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

-- gets the best out of the ... so if u have 1, 5, '1e50' it gets the '1e50'
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

-- clamps val = 0, min {man=0, exp=0}, max = BN max
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

-- rounds BN to its nearest val like {man = 1.46, exp = 2} rounds to {man = 1.5, exp = 2}
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

-- like math.exp
function Bn.exp(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 then return one end
	if val.exp == inf then return {man=1, exp=inf} end
	if val.exp ~= val.exp then return {man=1, exp = nan} end
	local l10e = Bn.fromNumber(0.4342944819032518)
	local pow = Bn.mul(val, l10e)
	return Bn.pow10(pow)
end

--basically like math.mod but for BN
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

--basically like math.modf but for BN
function Bn.modf(val: any): (BN, BN)
	val = Bn.convert(val)
	if val.man == 0 and val.exp == inf or val.exp ~= val.exp then
		return val, zero
	end
	local int = Bn.floor(val)
	local frac = Bn.sub(val, int)
	return int ,frac
end

--basically like math.fmod but for BN
function Bn.fmod(val1: any, val2: any): BN
	local q = Bn.div(val1, val2)
	local qF = Bn.floor(q)
	local prod = Bn.mul(qF, val2)
	return Bn.sub(val1, prod)
end

-- basically val^log10(2)
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

--creates it so if 100/1000 shows as 10%
function Bn.Percent(val1: any, val2: any): string
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val2.man == 0 then return '100%' end
	local ratio = Bn.div(val1, val2)
	local percent = Bn.mul(ratio, 100)
	if percent.man < 0 or percent.exp < 0 then
		return '0%'
	end
	local hund = Bn.fromNumber(1e2)
	if Bn.meeq(percent, hund) then return '100%' end
	return Bn.format(percent) .. '%'
end

function normalize(exp: number, layer: number): (number, number)
	if exp <= 308 then
		return exp, layer
	end
	return normalize(math.log10(exp), layer+1)
end

-- converts BN to HyperNum so {man = 1, exp = math.log(308), layer = 0+1} = HN
function Bn.toHN(val: any): HN
	val = Bn.convert(val)
	local layer
	local man: number, exp: number = val.man, val.exp
	if exp ~= exp then
		return {man = 0, exp = 0, layer = -1}
	end
	if exp == math.huge then
		return {man = 1, exp = math.huge, layer = math.huge}
	end
	if man == 0 then
		return {man = 0, exp = 0, layer = 0}
	end
	exp, layer = normalize(exp, 0)
	local shift = math.floor(math.log10(math.abs(man)))
	man = man / 10^shift
	exp = exp + shift
	return {man=man, exp=exp, layer=layer}
end

--converts BN to showable time
function Bn.timeConvert(val: any): string
	val = Bn.convert(val)
	local seconds = Bn.toNumber(val)
	if seconds < 0 then return "0s" end
	local days = math.floor(seconds / 86400)
	local hours = math.floor((seconds % 86400) / 3600)
	local minutes = math.floor((seconds % 3600) / 60)
	local secs = math.floor(seconds % 60)
	local parts = {}
	if days > 0 then table.insert(parts, days .. "d") end
	if hours > 0 then table.insert(parts, hours .. "h") end
	if minutes > 0 then table.insert(parts, minutes .. "m") end
	if secs > 0 or #parts == 0 then table.insert(parts, secs .. "s") end
	return table.concat(parts, ":")
end

--computes a log growth step: log10(val + 10^(sqrt(log10(val+10))))
function Bn.HyperRootLog(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 then return zero end
	local ten = Bn.fromNumber(10)
	local valPlus = Bn.add(val, ten)
	local logPart = Bn.log10(valPlus)
	local val_sqrt = Bn.sqrt(logPart)
	local component = Bn.pow10(val_sqrt)
	local result = Bn.add(val, component)
	return Bn.log10(result)
end

return Bn