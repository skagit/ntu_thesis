# a network has agents, edges, an adjacency list representation of the graph, information about size, topology, and replication id number used for recording data, a world, and a storage for cached data between intergenerational transfers
mutable struct Network
  agents :: Vector{Agent}
  ties :: Vector{Tuple{Int64, Int64}}
  knows :: Dict{Int64, Vector{Int64}}
  size :: Int64
  topology :: String
  id :: Int64
  world :: World
  data :: Vector{Tuple{Int64, Int64}}
end

#build the edge-list representation
function init_knows(ties, n_agents)
    knows = Dict()
    for id in 1:n_agents
        knows[id] = []
    end
    for (i, j) in ties
        knows[i] = union(knows[i] , [j])
        knows[j] = union(knows[j] , [i])
    end
    return knows
end

#create a network with a random graph structure
function init_pcon_network(p, n, world)
  agents = [ init_agent() for i in 1:n ]
  ties = pcon(p, n)
  knows = init_knows(ties, n)
  return Network(agents, ties, knows, n, string(p), 0, world, [])
end

#create a network with a Hierarchical structure
function init_hier_network(p, n, world)
  agents = [ init_agent() for i in 1:n ]
  ties = hier(n)
  knows = init_knows(ties, n)
  return Network(agents, ties, knows, n, string(p), 0, world, [])
end

#simulate intergenerational transfer processes and store productions for calculating synthesis
function init_productions!(net)
  for agent in net.agents
    seen = []
    for stimuli in net.world.stimuli

      pfs = produce(agent.grammar, stimuli)
      pfs = [ pfs[2] ; pfs[1] ]
      if length(pfs) < 1
        next = PF( Rule("S", Meaning(stimuli), generate_signal(net.world)), ["S"] )
      elseif length(pfs) > 0
        next = deepcopy(pfs[1])
      end

      filter!(isterminal, next.rule.signal)
      next.rule.meaning = Meaning(deepcopy(stimuli))

      while next.rule.signal in seen
        next.rule.signal = push!([next.rule.signal[1:end-2]; taken(generate_signal(net.world), 2)])
      end

      push!(agent.pfs, next)
      push!(seen, next.rule.signal)

      agent.says[stimuli] = push!(get!(agent.says, stimuli, []), deepcopy(next.rule))
      push!(agent.pfs, next)
      push!(seen, next.rule.signal)
    end

    counts = Dict{Sign, Int64}()
    for pf in agent.pfs
      for sign in pf.derivation
        counts[sign] = get(counts, sign, 0) + 1
      end
    end
    for (sign, count) in collect(counts)
      if length(sign) > 2 && sign[1:2] == "SG" && rand() <= (count * 0.001)
        for pf in agent.pfs
          if sign in pf.derivation
            target = pf.rule.meaning
            sig = agent.says[target][1].signal
            if length(sign) > 1
              sig = sig[1:end-1]
            else
              sig = generate_signal(net.world)
            end
          end
        end
      end
    end

    if rand() <= 0.1
      mean = agent.says[rand(net.world.stimuli)][1].meaning
      if length(mean) > 1
        pop!(mean)
      end
    end

    agent.knows = deepcopy(agent.says)
  end
end

#conduct an intergenerational transfer for all agents and prepare for another round
function transfer!(net)
  for agent in net.agents
    agent.grammar = grids( agent.input )
    agent.input = []
    agent.pfs = []
    agent.says = Dict{Meaning, Rule}()
  end
end

#run through a round of communication
function advance_round!(net)
  for i in 1:length(net.agents)
    j = rand(net.knows[i])
      stimuli = rand(net.world.stimuli)
      agent_i = net.agents[i]
      agent_j = net.agents[j]
      stim_i = rand(agent_i.says[stimuli])
      stim_j = rand(agent_j.says[stimuli])

      match = false
      for rule_i in agent_i.knows[stimuli]
        for rule_j in agent_j.says[stimuli]
          if rule_i == rule_j || levenshtein(rule_i.signal, rule_j.signal) < 2
            push!(agent_i.input, stim_j)
            if rand() < 0.25
              if !(stim_j in agent_i.says[stimuli])
                push!(agent_i.says[stimuli], stim_j)
              end
            end

            if !(stim_j in agent_i.knows[stimuli])
              push!(agent_i.knows[stimuli], stim_j)
            end
            match = true
            break
          end
        end
        if match
          break
        end
      end
      if match == false
        new_rule = Rule("S", Meaning(deepcopy(stimuli)), generate_signal(net.world))
        push!(agent_i.knows[stimuli], new_rule)
        push!(agent_i.says[stimuli], new_rule)
        push!(agent_j.knows[stimuli], new_rule)
        push!(agent_j.says[stimuli], new_rule)
      end

      match = false
      for rule_j in agent_j.knows[stimuli]
        for rule_i in agent_i.says[stimuli]
          if rule_i == rule_j || levenshtein(rule_i.signal, rule_j.signal) < 2
            push!(agent_j.input, stim_i)
            if rand() < 0.25
              if !(stim_i in agent_j.says[stimuli])
                push!(agent_j.says[stimuli], stim_i)
              end
            end

            if !(stim_i in agent_j.knows[stimuli])
              push!(agent_j.knows[stimuli], stim_i)
            end
            match = true
            break
          end
        end
        if match
          break
        end
      end
      if match == false
        new_rule = Rule("S", Meaning(deepcopy(stimuli)), generate_signal(net.world))
        push!(agent_i.knows[stimuli], new_rule)
        push!(agent_i.says[stimuli], new_rule)
        push!(agent_j.knows[stimuli], new_rule)
        push!(agent_j.says[stimuli], new_rule)
      end
  end
end

#simulate one generation of diffusion and intergenerational transfer, also record synthesis at beginning of generation
function advance_generation!(net, rounds)
  init_productions!(net)
  record!(net)
  for round in 1:rounds
    advance_round!(net)
  end
  transfer!(net)
end

#for a given network, number of generations and rounds, simulate one replication
function run!(net, gens, rounds)
  for gen in 1:gens
    println("gen ", gen)
    advance_generation!(net, rounds)
  end
  outname = join([net.topology, net.size, net.id], "_")
  outfile = open(string("tmp/", outname, ".csv"), "w")
  for (gen, pair) in enumerate(net.data)
    ms = pair[1]
    ws = pair[2]
    dataline = join([net.topology, net.size, net.id, gen, ms, ws, ms/ws], ", ")
    outline = string(dataline, "\n")
    write(outfile, outline)
  end
  close(outfile)
end

#calculate the synthesis of the language for a generation
function record!(net)
  total_ms = 0
  total_ws = 0
  for agent in net.agents
    (agent_ms, agent_ws) = calculate_ratios([pf.derivation for pf in agent.pfs], agent.grammar)
    total_ms += agent_ms
    total_ws += agent_ws
  end
  push!(net.data, (total_ms, total_ws))
end

#calculate the synthesis used by a single agent
function calculate_ratios(derivations, grammar)
(phrases, words, clitics, morphemes) = analyze_grammar(grammar)
    combined = [words; clitics]
    ws = 0
    ms = 0
    for derivation in derivations
        for sign in derivation
            if sign in combined
                ws += 1
            end
            if sign in morphemes
                ms += 1
            end
        end
    end
    return ms, ws
  end

# simple, iterative, edit-distance calculation
function levenshtein(s, t)
    ls, lt = length(s), length(t)
    if ls > lt
        s, t = t, s
        ls, lt = lt, ls
    end
    dist = collect(0:ls)
    for (ind2, chr2) in enumerate(t)
        newdist = Vector{Int}(ls+1)
        newdist[1] = ind2
        for (ind1, chr1) in enumerate(s)
            if chr1 == chr2
                newdist[ind1+1] = dist[ind1]
            else
                newdist[ind1+1] = 1 + min(dist[ind1], dist[ind1+1], newdist[end])
            end
        end
        dist = newdist
    end
    return dist[end]
end
