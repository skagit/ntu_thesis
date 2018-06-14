#a grammar is an array of rules
const Grammar = Vector{Rule}

#a production frame keeps track of a rule as it is being produced, as well as its derivatoin
mutable struct PF #production frame
  rule :: Rule
  derivation :: Signal
end

#a PF can be made by copying a rule and the rules symbol
function init_PF(rule)
  return PF(deepcopy(rule), deepcopy([rule.symbol]))
end
import Base.deepcopy

#internal function to deepcopy with new pointers to newly allocated data
function deepcopy(pf::PF)
  return PF(deepcopy(pf.rule), deepcopy(pf.derivation))
end

#filter the grammar for start rules
function getstarts(grammar)
  return filter( isstart, grammar )
end

#filter the grammar for rules that do not contain morpheme/sememe content
function getnonsegments(grammar)
  return filter(isnonsegment, grammar)
end

#filter the grammar to get all the rules of a particular symbol
function getrules(grammar, symbol)
  return filter(grammar) do rule
    rule.symbol == symbol
  end
end

#given a rule and a grammar, expand the non-terminals in the rule by one step each possible way
function expand(grammar::Grammar, base::Rule)
  #get possinel nonterminals
  nonterminals = [sign for sign in base.signal if isnonterminal(sign)]

  #exit if there are none
  if isempty(nonterminals)
    return []
  end

  #otherwise get possible rules that can expand those nonterminals
  expandables = [ getrules(grammar, sign) for sign in nonterminals ]

  result = []
  #for each set of expandable rules found for each nonterminal
  for rule_list in expandables
    for rule in rule_list
      #expand it using the rule, and collect it in results
      push!(result, expand(base, rule))
    end
  end

  return result
end

#expand the rule of a PF while updating the derivation
function expand_pf(grammar::Grammar, base::PF)
  #get possinel nonterminals
  nonterminals = [sign for sign in base.rule.signal if isnonterminal(sign)]

  #exit if there are none
  if isempty(nonterminals)
    return []
  end

  #otherwise get possible rules that can expand those nonterminals
  expandables = [ getrules(grammar, sign) for sign in nonterminals ]
  result = []

  #for each set of expandable rules found for each nonterminal
  for rule_list in expandables
    for rule in rule_list
      #expand it using the rule, and collect it in results
      newpf = deepcopy(base)
      push!(newpf.derivation, rule.symbol)
      newpf.rule = expand(base.rule, rule)
      push!(result, newpf)
    end
  end

  return result
end

#determine if a rule can be continued in production towards a target meaning
function cancontinue(rule::Rule, meaning::Meaning)
  meaning_terminals = filter(isterminal, rule.meaning)
  return aresubset(meaning_terminals, meaning)
end

#given a grammar and target meaning, produce all possible rules that match the meaing or are close
function produce(grammar, meaning)
  #get all possible start rules to continue from and set matches to empty
  return produce(grammar, meaning, [ init_PF(r) for r in getstarts(grammar)], [], 1)
end

#given a grammar and target meaning, produce all possible rules that match the meaing or are close
function produce(grammar, meaning, possibles, matches, limit)
  expansions = []

  #examine each possible rule
  for pf in possibles
    #if it matches, then add it to matches and move on
    if aresame(pf.rule.meaning,  meaning)
      push!(matches, pf)
      continue
    end

    #otherwise, expand the rule
    nexts = expand_pf(grammar, pf)

    #and keep any expansions that might yield or equal the meaning
    for next in nexts
      #if cancontinue(next.rule, meaning)
        push!(expansions, next)
      #end
    end
  end

  #when there are no more expansions, return any rules that came as close as possible along with any matches
  #cancontinue yields a subset so any errant meanings will be discarded
  #canexpand (in expand) checks for remaining nonterminals
  #so either the meaning matches, is a subset and runs out of nonterminals, or goes out of boudnds
  #all possible cases are accounted for and so the algorithm terminates

  if isempty(expansions) || limit == 5
    scored = map(possibles) do pf
      score = 0
      for mean in pf.rule.meaning
        if mean in meaning
          score += 10
        end
      end

      score, pf
    end

    sort!( scored, by=x->x[1], rev=true)
    possibles = [pf for (score, pf) in scored]

    foreach(possibles)do pf
      for sign in pf.rule.signal
        if isnonterminal(sign)
          push!(pf.derivation, sign)
        end
      end
    end

    foreach(matches)do pf
      for sign in pf.rule.signal
        if isnonterminal(sign)
          push!(pf.derivation, sign)
        end
      end
    end

    return unique(possibles), unique(matches)

  else
    return produce(grammar, meaning, unique(expansions), matches, limit + 1)
  end
end
