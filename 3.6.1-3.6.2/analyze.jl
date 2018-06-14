#a simple directed grap to store the rewrite dependency graph discussed in Chapter 3
struct DirectedGraph
    edges :: Set{Tuple{String, String}}
    nodes :: Set{String}
end

#build the graph from the symbols used in the ARS grammar
function rulestograph(grammar)
    graph = DirectedGraph(Set([]),Set([]))
    for rule in grammar
        push!(graph.nodes, rule.symbol)
        for sign in rule.signal
            push!(graph.edges, (rule.symbol, sign ))
        end
    end
    return graph
end

#find the rules that are children of the start symbol
function gettops(graph)
    tops = []
    for edge in graph.edges
        if edge[1] == "S"
            push!(tops, edge[2])
        end
    end
    return collect(Set(tops))
end

#get the segments
function getleafs(graph)
    leafs = []
    for node in graph.nodes
        edges = filter(graph.edges) do edge
            edge[1] == node
        end
        rights = map(edges) do edge
            edge[2]
        end
        if all(x->islower(x[1]), rights)
            push!(leafs, node)
        end
    end
    return leafs
end

#test to see if a symbol is recursive 
function isrecursive(node, graph)
    edges = filter(graph.edges) do edge
        edge[1] == node
    end
    rights = map(edges) do edge
        edge[2]
    end
    return any(sign->sign == node, rights)
end

#test to see what other rules in the graph can rewrite to a symbol
function hasotherparent(node, parent, graph)
  edges = filter(graph.edges) do edge
      edge[2] == node && edge[1] != parent
  end
  if length(edges) > 0
    return true
  else
    return false
  end
end

#use the rewrite dependency graph to find out which symbols violate the constraints to be a word
function violatesconstraints(graph)
  nogood = []
  for node in graph.nodes
   if isrecursive(node, graph)
    push!(nogood, node)
   end
  end
  for node in graph.nodes
    edges = filter(graph.edges) do edge
        edge[1] == node && isupper(edge[2][1])
    end
    rights = map(edges) do edge
        edge[2]
    end
    if any(right-> hasotherparent(right, node, graph), rights)
      push!(nogood, node)
    end
  end
  return collect(Set(nogood))
end

#get the the possible candidate for a rule to rewrite to
function getchildren(node, graph)
  edges = filter(graph.edges) do edge
      edge[1] == node
  end
  rights = map(edges) do edge
      edge[2]
  end
  return filter!(right-> right != node, collect(Set(rights)))
end

#analyze the ARS system using a rewrite dependency graph and return the syntactic transformations, word class, possible clitics, and segments
function analyze_grammar(grammar, state = ([],[],[],[],[],[]) , graph = false )
  (phrases, words, clitics, morphemes, wops, nonword) = state
  if graph == false
    graph = rulestograph(grammar)
    nonword = violatesconstraints(graph)
    wops = gettops(graph)
    morphemes = getleafs(graph)
  end

  if length(wops) == 0
    return (unique(phrases), unique(words), unique(clitics), unique(morphemes))
  else
    nexts = []

    for wop in wops
      if wop in nonword
        push!(phrases, wop)
        children = getchildren(wop, graph)
        for child in children
            push!(nexts, child)
        end
      else
        push!(words, wop)
      end
    end
    nexts = filter(nexts) do item
        if item in wops
            return false
        end
	    return true
    end

    return analyze_grammar(grammar,
                          (phrases, words, clitics, morphemes, nexts, nonword),
                          graph)
  end
end
