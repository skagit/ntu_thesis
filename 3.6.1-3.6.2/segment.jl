#counter for segment rule ids
global next_segment_number = 1

#a support of associated data has the prefix and suffix appearing around the search frames signal that remain, as well as remaining meaning
mutable struct Support
  pre  :: Signal
  suff :: Signal
  mean :: Meaning
end

#a search frame has the meaning and signal being currently evaluated
#in addition it keeps track of rules it is embedded in (support), the other data items from the rule the location was found in (cosupprt), as well as its MDL estimate, and status to if it can still extend the meaning or signal
mutable struct SearchFrame
  meaning :: Meaning
  signal :: Signal
  support :: Vector{Support}
  cosupport :: Vector{Support}
  information :: Int64
  can_mean :: Bool
  can_sig :: Bool
end

#julia deepcopy implementation to prevent multiple pointers to data
import Base.deepcopy
function deepcopy(a::Support)
  Support( deepcopy(a.pre), deepcopy(a.suff), deepcopy(a.mean))
end

#julia deepcopy implementation to prevent multiple pointers to data
function deepcopy(a::SearchFrame)
  SearchFrame( deepcopy(a.meaning), deepcopy(a.signal), deepcopy(a.support), deepcopy(a.cosupport), deepcopy(a.information), deepcopy(a.can_mean), deepcopy(a.can_sig))
end

#given a list, take up to n items from the front if possible
function taken(lst, n)
  if length(lst) >= n
    return lst[1:n]
  else
    return lst
  end
end

#is the rule not completely segmented
function isunsegmented(rule::Rule)
  any(isterminal, rule.meaning) && any(isterminal, rule.meaning)
end

#given a set of paritally segment input rules, convert them to data rules for searching
function rulestodata(grammar::Grammar)
  data = Vector{Rule}()
  for (num, rule) in enumerate( filter( isunsegmented, grammar))
    next_meaning = filter(isterminal, rule.meaning)
    next_signal = Vector{Sign}()
    for sign in rule.signal
      if isterminal(sign)
        push!(next_signal, sign)
      elseif isnonterminal(sign) && length(next_signal) > 0
        next_datum = Rule(string(num), deepcopy(next_meaning), next_signal)
        push!(data, next_datum)
        next_signal = Vector{Sign}()
      end
    end
    if length(next_signal) > 0
      next_datum = Rule(string(num), deepcopy(next_meaning), next_signal)
      push!(data, next_datum)
    end
  end
  return data
end

#count the signal signs in the data rules
function count_signals(data::Grammar)
  counts = Dict{Sign, Int64}()
  for rule in data
    for sign in rule.signal
      counts[sign] = get(counts, sign, 0) + 1
    end
  end
  sort!( collect(counts), by = x->x[2], rev = true)
end

#convenience function to get the lenghts of various support items
function len_support(support)
  return length(support.pre) + length(support.suff), length(support.mean)
end

#dose the current search frame occur in more than one place, and if it were segmented, would it prevent other segments in the rule it originated in from being made
function is_valid_segment(sf)
  if length(sf.support) < 2
    return false
  end
  for support in sf.cosupport
    ls, lm = len_support(support)
    if lm <= 0
      return false
    end
  end

  return true
end

#given a paritally segmente input, get the initial search frames
function get_initial_sfs(grammar)
  data = rulestodata(grammar)
  counts = count_signals(data)
  signs = [ pair[1] for pair in taken(counts, 3) ]

  signal_sfs = Vector{SearchFrame}()

  for sign in signs
    support = Vector{Support}()
    conums = Vector{String}()
    cosupport = Vector{Support}()
    for rule in data
      for (i, val) in enumerate(rule.signal)
        if val == sign
          pre  = rule.signal[1 : i - 1]
          suff = rule.signal[i + 1 : end]
          push!(support, Support(deepcopy(pre), deepcopy(suff), deepcopy(rule.meaning)))
          push!(conums, rule.symbol)
        end
      end
    end
    for conum in conums
      for rule in data
        if rule.symbol == conum && !(sign in rule.signal)
          push!(cosupport, Support([], [], deepcopy(rule.meaning)))
        end
      end
    end
    push!(signal_sfs, SearchFrame([], [sign], support, cosupport, 0, true, true))
  end

  meaning_sfs = Vector{SearchFrame}()

  for sf in signal_sfs
    meanings = Set{Sign}()
    for support in sf.support
      for meaning in support.mean
        push!(meanings, meaning)
      end
    end

    for meaning in meanings
      newsf = deepcopy(sf)
      newsf.support = filter(s -> meaning in s.mean, newsf.support)
      newsf.cosupport = filter(s -> meaning in s.mean, newsf.cosupport)

      foreach(s -> remove!(s.mean, meaning), newsf.support)
      foreach(s -> remove!(s.mean, meaning), newsf.cosupport)

      push!(newsf.meaning, meaning)

      newsf.information = length(newsf.support) * ((length(newsf.meaning) + length(newsf.signal)) - 0 ) #remove cost of rule symbol as each initial morpheme would evaluate to 0 otherwise

      push!(meaning_sfs, newsf)
    end
  end


  filter!(is_valid_segment, meaning_sfs)
  return meaning_sfs
end

#expand a search frame each possible way it can be done using the current support
#that is, extend the signal one sign outwards on each end
#and add any possible new meaning sememes
function step_searchframe(sf)
  nexts = Vector{SearchFrame}()

  if sf.can_mean
    meanings = Set{Sign}()
    for support in sf.support
      for meaning in support.mean
        push!(meanings, meaning)
      end
    end


    for meaning in meanings
      newsf = deepcopy(sf)
      newsf.support = filter(s -> meaning in s.mean, newsf.support)
      newsf.cosupport = filter(s -> meaning in s.mean, newsf.cosupport)

      foreach(s -> remove!(s.mean, meaning), newsf.support)
      foreach(s -> remove!(s.mean, meaning), newsf.cosupport)

      push!(newsf.meaning, meaning)

      newsf.information = length(newsf.support) * ((length(newsf.meaning) + length(newsf.signal)) - 2 )


      push!(nexts, newsf)
    end
  end

  if sf.can_sig
    prefixes = Set{Sign}()
    suffixes = Set{Sign}()
    for support in sf.support
      if length(support.pre) > 0
        push!(prefixes, support.pre[end])
      end

      if length(support.suff) > 0
        push!(suffixes, support.suff[1])
      end
    end

    for prefix in prefixes
      newsf = deepcopy(sf)
      newsf.support = filter(s -> length(s.pre) > 0 && s.pre[end] == prefix, newsf.support)
      foreach(s -> pop!(s.pre), newsf.support)
      unshift!(newsf.signal, prefix)
      newsf.information = length(newsf.support) * ((length(newsf.meaning) + length(newsf.signal)) - 2 )
      push!(nexts, newsf)
    end

    for suffix in suffixes
      newsf = deepcopy(sf)
      newsf.support = filter(s -> length(s.suff) > 0 && s.suff[1] == suffix, newsf.support)
      foreach(s -> shift!(s.suff), newsf.support)
      push!(newsf.signal, suffix)
      newsf.information = length(newsf.support) * ((length(newsf.meaning) + length(newsf.signal)) - 2 )
      push!(nexts, newsf)
    end
  end

  filter!(is_valid_segment, nexts)
  return nexts
end

#go through the partially segmented grammar to get best segment in terms of MDL
function search_segment(grammar)
  initials = taken(get_initial_sfs(grammar), 3)
  foreach(sf -> sf.information = length(sf.support) * ((length(sf.meaning) + length(sf.signal)) - 2 ), initials)
  return search_segment(grammar, initials, initials)
end

#go through the partially segmented grammar to get best segment in terms of MDL
function search_segment(grammar, seen, searching)
  if isempty(searching)
    return seen
  else
    searching = mapreduce(step_searchframe, vcat, [], searching)
    threshold = minimum(  map(seen) do sf
                            sf.information
                          end )
    filter!(sf->sf.information >= threshold, searching)
    seen = taken( sort!( vcat(seen, searching), by=x->x.information, rev=true) , 3)

    return search_segment(grammar, seen, taken(searching, 15) )
  end
end

#continue to find segments, segment grammar, and finalize the segmentation when finished
function segment(grammar, segments = [])
  global next_segment_number
  next_segments = search_segment(grammar)
  if isempty(next_segments)
    finalize_segmentation(grammar, segments)
  else
    next_segment = next_segments[1]
    newsegment = Rule( "SG"*string(next_segment_number), next_segment.meaning, next_segment.signal)
    next_segment_number += 1
    push!(segments, newsegment)

    for (i, rule) in enumerate(grammar)
      grammar[i] = contract(rule, newsegment)
    end
    return segment(grammar, segments)
  end
end

#if any meaning and signal could not be segmented -- the pattern did not occur in more than one input -- create a new segment to account for it and segment the input fully
function finalize_segmentation(grammar, segments)
  global next_segment_number
  new_rules = Grammar()
  for rule in grammar
    if ismorpheme(rule)
      newsegment = Rule( "SG"*string(next_segment_number), filter(isterminal, rule.meaning), filter(isterminal, rule.signal) )
      next_segment_number += 1
      push!(segments, newsegment)
      rule.meaning = [ filter(isnonterminal, rule.meaning) ; [newsegment.symbol] ]
      rule.signal = [ filter(isnonterminal, rule.signal) ; [newsegment.symbol] ]
      push!(new_rules, rule)
      continue
    else
      rule.meaning = filter(isnonterminal, rule.meaning)
      rule.signal  =  filter(isnonterminal, rule.signal)
      push!(new_rules, rule)
    end
  end
  return Grammar([new_rules; segments])
end
