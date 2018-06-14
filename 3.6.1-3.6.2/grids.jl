# a search frame for use in patern searching with MDL
# the symbols are the currently found pattern, the support the rules that contain the pattern, and infomation and MDL estimate of the change in compression if the pattern is abstracted
mutable struct GRIDSSF
  symbols :: Signal
  support :: Grammar
  information :: Int64
end

#counter for rule ids
global next_rule_number = 1

#return n-grams of a signal
function findngrams(signal, n)
  if length(signal) < n
    return []
  end
  ngrams = Vector{Signal}()
  for start in 1: (length(signal) - n) + 1
    push!(ngrams, signal[start: start + (n-1)])
  end
  return ngrams
end

#search the ARS rules and return the best pattrn in terms of informatic gain
function getpatterns(grammar)
  rules = getnonsegments(grammar)
  n = 2
  cangram = true
  sfs = Vector{GRIDSSF}()
  while cangram
    counts = Dict{Signal, Vector{Rule}}()
    for rule in rules
      grams = findngrams(rule.signal, n)
      for gram in grams
        support = get!(counts, gram, [])
        push!(support, rule)
      end
    end

    cangram = false
    for (gram, support) in counts
      if length(support) < 2
        continue
      end
      cangram = true
      push!(sfs, GRIDSSF(gram, support, length(gram) * length(support)))
    end
    if cangram == true
      n += 1
    end
  end
  taken( sort!( sfs, by=x->x.information, rev=true) , 5)
  return sfs
end

#continue to search the ARS rules until no more patterns can be found
function abstract_patterns(grammar, altered)
  global next_rule_number
  next_patterns = getpatterns(grammar)
  if isempty(next_patterns)
    return grammar, altered
  else
    altered = true
    next_pattern = next_patterns[1]
    newpattern = Rule( "R"*string(next_rule_number), Meaning(next_pattern.symbols), Signal(next_pattern.symbols))
    next_rule_number += 1
    newrules = Vector{Rule}()
    for rule in deepcopy(grammar)
      if cancontract(rule, newpattern)
        push!(newrules, contract(rule, newpattern))
      else
        push!(newrules, rule)
      end
    end
    push!(newrules, newpattern)

    return abstract_patterns(newrules, altered)
  end
end

#take a meaning and replace the old sign with the new sign
function replace_symbol!(meaning::Meaning, ns, os)
  if os in meaning
    remove!(meaning, os)
    add!(meaning, ns)
  end
end

#take a signal and replace the old sign with the new sign 
function replace_symbol!(signal::Signal, ns, os)
  if os in signal
    for i in 1:length(signal)
      if signal[i] == os
        signal[i] = ns
      end
    end
  end
end

#replace the old sign with the new sign in a rule
function replace_symbol!(rule::Rule, ns, os)
  replace_symbol!(rule.meaning, ns, os)
  replace_symbol!(rule.signal, ns, os)
  if rule.symbol == os
    rule.symbol = ns
  end
end

#replace the old sign with the new sign in a whole ARS grammar
function replace_symbol!(grammar::Grammar, ns, os)
  for rule in grammar
    replace_symbol!(rule, ns, os)
  end
end

#search for generalizations and update the ARS grammar until no more generalizations can be found
function generalize_signs(grammar, altered)
  units = [ rule for rule in grammar if rule.symbol != "S" && !(issegment(rule)) && length(rule.meaning) == 1 && length(rule.signal) == 1 && rule.symbol != rule.meaning[1] ]
  if length(units) > 0
    altered = true
    new_sign =  units[1].meaning[1]
    old_sign = units[1].symbol
    replace_symbol!(grammar, new_sign, old_sign)

    return generalize_signs(grammar, altered)
  end

  nonstarts = [ rule for rule in grammar if !(isstart(rule))]
  for i in 1:(length(nonstarts) - 1)
    for j in i:length(nonstarts)
      rule_a = nonstarts[i]
      rule_b = nonstarts[j]
      if  rule_a.signal == rule_b.signal && rule_a.meaning == rule_b.meaning && rule_a.symbol != rule_b.symbol
        altered = true
        new_sign = rule_a.symbol
        old_sign = rule_b.symbol
        replace_symbol!(grammar, new_sign, old_sign)
        return generalize_signs(grammar, altered)
      end
    end
  end

  nonsegments = [rule for rule in grammar if !(issegment(rule))]
  for i in 1:(length(nonsegments) - 1)
    for j in i:length(nonsegments)
      rule_a = nonsegments[i]
      rule_b = nonsegments[j]

      if rule_a.symbol == rule_b.symbol &&  length(meandiff(rule_a.meaning, rule_b.meaning)) == 1 && length(meandiff(rule_b.meaning, rule_a.meaning)) == 1 && !(aresubset(rule_b.meaning, rule_a.meaning))

        new_sign = meandiff(rule_a.meaning, rule_b.meaning)[1]
        old_sign = meandiff(rule_b.meaning, rule_a.meaning)[1]

        match = true

        if length(rule_a.signal) + length(rule_b.signal) > 3 && length(rule_a.signal) == length(rule_b.signal)
          misses = []
          for (i,v) in enumerate(rule_a.signal)
            w = rule_b.signal[i]
            if v == w
              continue
            elseif (v == new_sign && w == old_sign) || (w == new_sign && v == old_sign)
              push!(misses, (v,w) )
              continue
            else
              match = false
              break
            end
          end
          if length(misses) != 1
            match = false
          end

          if match
            altered = true
            replace_symbol!(grammar, new_sign, old_sign)
            return generalize_signs(grammar, altered)
          end
        end
      end
    end
  end

  return grammar, altered
end

#given a raw input, segment the input and then abstract and genralize as much as possible
function grids(rules)

  segmented = segment(rules)

  grids(segmented, false)
end

#given a raw input, segment the input and then abstract and genralize as much as possible
function grids(grammar, altered)
  (abstracted, hasabstraction) = abstract_patterns(grammar, altered)
  (generalized, hasgeneralization) = generalize_signs(abstracted, hasabstraction)

  if hasabstraction || hasgeneralization
    return grids(generalized, false)
  else
    return unique(grammar)
  end
end
