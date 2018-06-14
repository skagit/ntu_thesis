#a sign used to represent sememes, phonemes, and rule symbols
const Sign = String

#the start sign of rewrite rules
const StartSign = "S"

#the empty sign
const EmptySign = ""

#terminals begin with lowercase
function isterminal(sign::Sign)
    return islower(sign[1])
end

#nonterminals begin with uppercase
function isnonterminal(sign::Sign)
    return isupper(sign[1])
end
