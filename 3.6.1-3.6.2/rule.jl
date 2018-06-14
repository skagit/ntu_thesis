#an ARS rule has a left-hand side symbol and meaning and signal components on the right-hand side
mutable struct Rule
  symbol :: Sign
  meaning :: Meaning
  signal :: Signal
end

#a rule with no content
const EmptyRule = Rule(EmptySign, EmptyMeaning, EmptySignal)

#julia system notion of equality
import Base.==
function ==(a::Rule, b::Rule)
  a.symbol == b.symbol &&
  a.meaning == b.meaning &&
  a.signal == b.signal
end

#julia system hash function
import Base.hash
function hash(a::Rule)
  return hash(a.symbol) + hash(a.meaning) + hash(a.signal)
end

#julia system deepcopy to prevent multiple pointers to same data
import Base.deepcopy
function deepcopy(rule::Rule)
  return Rule( deepcopy(rule.symbol), deepcopy(rule.meaning), deepcopy(rule.signal) )
end

#for convenience, create a rule from three strings as input
#meaning and signal strings are the signs separated by spaces ex. StoR( "S", "N V", "NP VP")
function StoR(symbol, meaning, signal)
  Rule( symbol, Base.split(meaning), Base.split(signal))
end

#is this a start rule
function isstart(rule::Rule)
  return rule.symbol == StartSign
end

#is this a rule that is not a segment
function isnonsegment(rule::Rule)
  return all(isnonterminal, rule.meaning) && all(isnonterminal, rule.signal)
end

#is this rule a segment of sememe and phoneme content
function issegment(rule::Rule)
  return any(isterminal, rule.meaning) || any(isterminal, rule.signal)
end

#is this rule a morpheme composed only of sememe and phoneme content
function ismorpheme(rule::Rule)
    return any(isterminal, rule.meaning) && any(isterminal, rule.signal)
end

#can this base rule be expanded by substituting the meaning and signal of the other rule
function canexpand(base::Rule, rule::Rule)
  return rule.symbol in base.meaning && rule.symbol in base.signal
end

#substitute the meaning and signal of the rule into the base meaning and signal where appropriate
function expand(base::Rule, rule::Rule)
  if canexpand(base, rule)
    (pre, _ , suf) = split(base.signal, rule.symbol)
    newsignal = deepcopy([ pre ; rule.signal ; suf ])
    newmeaning = deepcopy(base.meaning)
    remove!(newmeaning, rule.symbol)
    add!(newmeaning, rule.meaning)
    return Rule(base.symbol, newmeaning, newsignal)
  end
  return base
end

#can the meaning and signal of the rule be found in the base
function cancontract(base::Rule, rule::Rule)
  (left_i, _) = findeyes(base.signal, rule.signal)
  return left_i != 0 && aresubset(rule.meaning, base.meaning)
end

#if the meaning and signal of the rule can be found in the base, replace them where appropriate with the rule's symbol
function contract(base::Rule, rule::Rule)
  if cancontract(base, rule)
    (pre, _ , suf) = split(base.signal, rule.signal)
    newsignal  = deepcopy([ pre ; rule.symbol ; suf ])
    newmeaning = deepcopy(base.meaning)
    for el in rule.meaning
      remove!(newmeaning, el)
    end
    add!(newmeaning, rule.symbol)
    return Rule(base.symbol, newmeaning, newsignal)
  end
  return base
end
