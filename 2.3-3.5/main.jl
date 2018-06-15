@everywhere begin
  ### DATA TYPES
mutable struct Construction
  # a Construction has a meaning, some original source signal pattern,
  # and a level or renanalysis
  meaning::Int64
  origin::Int64
  level::Int64
end
mutable struct Language
  # a Language is a collection of meanings and constructions
  # its primary importance is not to model human language, but to keep global variables such as id numbers coordinated over the agents, but local to the simulation in multiprocessing
  meanings::Int64
  origins::Int64
  construction_i::Int64
  constructions::Array{Construction,1}
end
mutable struct Experience
  # an Experience in the agetns history has the id of, and construction used by, the agent who spoke to the hearere
  agent::Int64
  construction::Int64
end
mutable struct Agent
  # an agent is a collection of active and passive Language knowledge,
  # and Experiences
  # active and passive are meaning => construction maps
  # both key and values are indicies to respective Lanuage components
  # of the Agent's Network
  active::Dict{Int64, Array{Int64,1}}
  passive::Dict{Int64, Array{Int64,1}}
  experience::Array{Experience, 1}
end
mutable struct Network
  # a Network consists of a collection of Agents, their undirected connections, a reference to the language, an adjacency list representation of the graph, and a label for recording purposes
  agents::Array{Agent, 1}
  connections::Array{Tuple{Int64, Int64}, 1} #(agent_index, agent_index) edges
  language::Language
  knows::Dict{Int64, Array{Int64,1}} #maps agent index to their other connected agents
  label::String #name of network
end

###TYPE CONSTRUCTION
#Connection Functions
function neighbors(v, edges)
  filter(edges) do pair
    (i,j) = pair
    if i == v || j == v
      return true
    else
      return false
    end
  end
end

#get all edges in a list of edges that the agent participates in
function localconnections(edges, id)
  return [ connection for connection in edges if connection[1] == id || connection[2] == id]
end

#create the network ties for a network of a given connection probability and size
#then ensure that each network is completely connected, as we are not considering multiple social networks in the same graph
function pcon(probability, n_agents)
  connections = []
  for i in 1:(n_agents - 1)
    for j in (i+1):n_agents
      if rand() <= probability
        push!(connections, (i,j) )
      end
    end
  end

  if length(connections) < 1
    push!(connections, (1,2) )
  end

  #after generating the stochastic connections, check that each agent has at least one connection
  starts = [ localconnections(connections, connections[1][1]) ; localconnections(connections, connections[1][2]) ]
  connected = unique( [ [edge[1] for edge in starts] ; [edge[2] for edge in starts] ] )
  for id in 1:n_agents
    if id in connected
      continue
    else
      new_partner = rand(connected)
      push!(connected, id)
      push!(connections, (id, new_partner) )
    end
  end
  return connections
end

#call pcon to generate random graph, but using same interface as other networks
function connect_random(agents, prob)
  n = length(agents)
  return pcon(prob, n)
end

#generate 25-agent Hierarchical ties
function connect_h5n2(agents)
  base = append!([ (1,2) , (2,3) , (3,4) , (4,1) ] , [(i, 5) for i in 1:4])
  externs = [(x+(5*i), y+(5*i)) for i in 1:4 for (x, y) in base]
  centrals = [(x + (5*i), 5) for x in 1:4 for i in 1:4]
  return append!(append!(base, externs), centrals)
end

#generate 125-agent Hierarchical ties
function connect_h5n3(agents)
  h5n2s = [connect_h5n2(agents) for i in 1:5]
  for i in 2:5
    for (j, (a, b)) in enumerate(h5n2s[i])
      h5n2s[i][j] = (a+(25*(i-1)) , b+(25*(i-1)))
    end
  end
  results = []
  for level in h5n2s
    append!(results, level)
  end
  for i in 30:50
    if i%5 !== 0
      push!(results, (i, 5) )
    end
  end
  for i in 55:75
    if i%5 !== 0
      push!(results, (i, 5) )
    end
  end
  for i in 80:100
    if i%5 !== 0
      push!(results, (i, 5) )
    end
  end
  for i in 105:125
    if i%5 !== 0
      push!(results, (i, 5) )
    end
  end
  results
end

#generate BA network
function connect_ba(agents)
  function assoc(array, index)
    for (i, val) in array
      if i == index
        return (i, val)
      end
    end
    return "error"
  end
  m = 1
  k = 2 + ((m-2) * 2) #links if initial nodes are connected linearly
  connections = []
  degrees = zeros(Int64, m)
  for i in 1:m-1
    push!(connections, (i, i+1))
    degrees[i] += 1
    degrees[i+1] += 1
  end
  for new_node in m+1:length(agents)
    prob = degrees ./ k
    ids = []
    for i in 1:length(prob)
      push!(ids, (i, prob[i]))
    end
    for c in 1:m
      sort!(ids, by=x->begin (a, b) = x; b end, rev=true)
      id, rem = assoc(ids, prob_pick_id(ids))
      push!(connections, (new_node, id))
      degrees[id] += 1
      ids = [ (index, prob) for (index, prob) in ids if index != id]
      ids = [ (id, prob + (rem / length(ids))) for (id, prob) in ids ]
    end
    push!(degrees, m)
    k += (m * 2)
  end
  connections
end

###Constructors
#given an array of (value, probability) pairs, where the probabilitites form a partition, select a value based on the collective probabilities
function prob_pick_id(array)
  target = rand()
  sum = 0
  for (i, prob) in array
    sum += prob
    if target <= sum
      return i
    end
  end
  return array[length(array)][1]
end

#generate a novel construction in the language, update the global variables in the language, and return the id of the construction
function novel_construction(language, meaning) #operates at Language level
  language.origins += 1
  language.construction_i += 1
  novel = Construction(meaning, language.origins, 0)
  push!(language.constructions, novel)
  return language.construction_i #index of added construction
end

#generate a renalyzed construction in the language, update the global variables in the language, and return the id of the reanalyzed construction
function evolve_construction(language, construction)
  old = language.constructions[construction]
  evolved = Construction(old.meaning, old.origin, old.level + 1)
  language.construction_i += 1
  push!(language.constructions, evolved)
  return language.construction_i
end

#create a new language with a set number of meanings and return a reference to it
function new_language(n_meanings)
  meanings = n_meanings
  origins = n_meanings
  construction_i = n_meanings
  constructions = [Construction(m,o,0) for (m,o) in zip(1:meanings, 1:origins) ]
  return Language(meanings, origins, construction_i, constructions)
end

#return a new newtork given the number of agents, number of meanings for the initial language, a reference to a function thunk to generate the ties, and a label for recording
function new_network(n_agents, n_meanings, connection_fn, label)
  language = new_language(n_meanings)
  base_map = Dict(k => [v] for (k,v) in zip(1:n_meanings, 1:n_meanings))
  agents = [Agent(base_map, base_map, []) for _ in 1:n_agents]
  connections = connection_fn(agents)
  knows = Dict(k => [] for k in 1:n_agents)
  for v in 1:n_agents
    partners = map(neighbors(v, connections)) do pair
      (i,j) = pair
      if i == v
        return j
      else
        return i
      end
    end
    for partner in partners
      push!(knows[v], partner)
    end
  end
  return Network(agents, connections, language, knows, label)
end

###METHODS
#Network
#progress the network through one round of communication
function step_network!(network)

  #function to have two agents share signals
  function converse(i, j)

    #can the hearer understand the target signal using their passive knowledge
    function canunderstand(target, hearer)
      knowns = hearer.passive[target.meaning]
      if target.level == 0 #paraphrastic phrases are always understood
        return true
      end
      for known in [network.language.constructions[index] for index in knowns]
        #phonetics share same origin, and changes not great, then understood
        if known.origin == target.origin && abs(known.level - target.level) <= 1
          return true
        end
      end
      return false
    end

    #given the active constructions of the speaker, can the hearer understand one to repair the conversation
    function repair(constructions, hearer)
      for index in constructions
        target = network.language.constructions[index]
        understood = canunderstand(target, hearer)
        if understood
          return (true, index)
        end
      end
      return (false, 0)
    end

    #simulate one have of the conversation with the agents taking on the role of either speaker or hearer
    function talkto(speaker, hearer)
      meaning = rand(1:network.language.meanings)
      spoken_i = rand(speaker.active[meaning])
      spoken = network.language.constructions[spoken_i]
      understood = canunderstand(spoken, hearer)
      learned = false
      bridge = 0
      if understood
        push!(hearer.experience, Experience(i, spoken_i)) #hearer knows speaker uses this construction
        if !(spoken_i in hearer.passive[meaning]) #hearer updates passive knowledge
          push!(hearer.passive[meaning], spoken_i)
        end
        if rand([true,false,false,false]) #one in four chance
          if !(spoken_i in hearer.active[meaning]) #to update active knowledge
            push!(hearer.active[meaning], spoken_i)
          end
        end
      else
        (learned, bridge) = repair(speaker.active[meaning], hearer) #if not understood, speaker can try to rephrase
      end
      if learned
        push!(hearer.experience, Experience(i, spoken_i)) #hearer knows speaker uses this construction
        #push!(hearer.experience, Experience(i, bridge)) #hearer knows speaker uses this bridge construction
        push!(hearer.passive[meaning], spoken_i) #become able to understand target
        # if !(bridge in hearer.passive[meaning]) #hearer updates passive knowledge with bridge
        # push!(hearer.passive[meaning], bridge)
        # end
      end
      if !(learned) && !(understood)
        invented = novel_construction(network.language, meaning) #returns index of new construction in language
        #add the novel construction to both active and passive knowledge of speaker and hearer
        push!(speaker.active[meaning], invented)
        push!(speaker.passive[meaning], invented)
        push!(hearer.active[meaning], invented)
        push!(hearer.passive[meaning], invented)
        #speaker and hearer experience eachother using the constuction
        push!(speaker.experience, Experience(j, invented))
        push!(hearer.experience, Experience(i, invented))
      end
    end
    agent_a = network.agents[i]
    agent_b = network.agents[j]
    talkto(agent_a, agent_b)
    talkto(agent_b, agent_a)
  end

  #select the conversations to happen this round of communication
  function select_events()
    return shuffle(network.connections)
    #return shuffle!([(v, rand(network.knows[v])) for v in 1:length(network.agents)])
  end

  events = select_events()
  for event = events
    (i,j) = event
    converse(i,j)
  end
end

#have network undergo an intergenerational transger
function evolve_network!(network)
  #have an agent select the new signals and simulate the process of transmission
  function evolve_agent(i, agent)
    #select the next active signal for a particular meaning from the set of constructions in the input used for that meaning
    function select_active(constructions, n_connections)
      candidates = unique(constructions)
      stats = map(candidates) do candidate
        count = length(filter(constructions) do construction
          construction == candidate
        end)
        (network.language.constructions[candidate].level, count, candidate)
      end
      sort!(stats, by = item -> begin
      (level, count, candidate) = item
      count
      end,
      rev=true)
      high = stats[1][2]
      threshed = filter(stats) do item
        (level, count, candidate) = item
        count == high
      end
      sort!(threshed, by = item -> begin
      (level, count, candidate) = item
      level
      end,
      rev=true)
      return shuffle(threshed)[1][3]
    end

    #get the next active signal for all meanings in the language based on the agent's experience
    function select_actives(experiences)
      selected = Dict( k => [] for k in 1:network.language.meanings)
      candidates = Dict( k => [] for k in 1:network.language.meanings)
      n_connections = length(network.knows[i])
      for experience in experiences
        construction = experience.construction
        meaning = network.language.constructions[construction].meaning
        push!(candidates[meaning], construction)
      end
      for (meaning, actives) in candidates
        if length(actives) < 1
          push!(candidates[meaning], rand(agent.active[meaning]) )
        end
      end
      for meaning in 1:network.language.meanings
        push!(selected[meaning], select_active(candidates[meaning], n_connections))
      end
      return selected
    end

    #for the selected actives simulate the processes of transmission identified in the grammaticalization literature
    function grammaticalize(next_actives, grammar_constant = .01)
      selected = Dict( k => [] for k in 1:network.language.meanings)
      for (meaning, actives) in next_actives
        active = actives[1]
        if network.language.constructions[active].level >= 20 && rand([false, false, true]) #check if too far leveled
          next = novel_construction(network.language, meaning)
          push!(selected[meaning], next)
          continue
        elseif network.language.constructions[active].level < 20 && rand() <= ( length(network.knows[i]) * grammar_constant )
          next = evolve_construction(network.language, active)
          push!(selected[meaning], next)
          continue
        else
          push!(selected[meaning], active)
        end
      end
      return selected
    end

    #populate the passive knowledge with the new active knoweldge post-transmission effects
    function select_passives(experiences, next_actives)
      selected = Dict( k => [] for k in 1:network.language.meanings)
      for experience in experiences
        construction = experience.construction
        meaning = network.language.constructions[construction].meaning
        push!(selected[meaning], construction)
      end
      for (meaning, actives) in next_actives
        for active in actives
          if !(active in selected[meaning])
            push!(selected[meaning], active)
          end
        end
      end
      return selected
    end

    exp_types = unique(agent.experience) do experience #return unique experiences, i.e. ignore how many times the same speaker used the same token
      (experience.agent, experience.construction)
    end
    next_actives = select_actives(exp_types) #pick next actives from experience
    grammaticalized = grammaticalize(next_actives) #return new possibly grammaticalized constructions
    next_passives = select_passives(exp_types, grammaticalized) #build passive knowledge from experienced constructions and grammaticalized actives
    return Agent(grammaticalized, next_passives, [])
  end

  for (i, agent) in enumerate(network.agents)
    next_agent = evolve_agent(i, agent)
    network.agents[i] = next_agent
  end
end

#given a number of agents, generations of transmission, rounds of diffusion, number of meanings, a connection function thunk, and label and replication number
#simulate a full replication of the network
function run_network(n_agents, n_gens, n_cycles, n_meanings, connection_fn, label, num)

  #record a generation worth of agent data into the output csv
  function record_status(net, outfile, gen, num)
    outstr = ""
    for (agent_id, agent) in enumerate(net.agents)
      for meaning in 1:net.language.meanings
        for construction in agent.active[meaning]
          level = net.language.constructions[construction].level
          origin = net.language.constructions[construction].origin
          size = 25
          if net.label[end] == 'x'
            size = 125
          end
          outstr *= "$(num), $(net.label), $(size), $(gen), $(agent_id), $(meaning), $(construction), $(origin), $(level)\n"
        end
      end
    end
    write(outfile, outstr)
  end

  outfile_name = "tmp/signal/$(label)_$(num).csv"
  outfile = open(outfile_name,"w")
  net = new_network(n_agents, n_meanings, connection_fn, label)

  for gen in 1:n_gens
    record_status(net, outfile, gen, num)
    for round in 1:n_cycles
      step_network!(net)
    end
    evolve_network!(net)
    println("$(label)_$(gen)")
  end

  outfile_ed = open("tmp/net/$(label)_$(num)_ed.csv", "w")
  edstr = ""
  for con in net.connections
    edstr *= "$(num), $(net.label), $(con[1]), $(con[2])\n"
  end
  write(outfile_ed, edstr)
  close(outfile_ed)
end

end

### MAIN ###

function simulation3()
   const connection_ps = [ 0.1, 0.2, 0.3, 0.4 ,0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
   const sizes = [25]
   const rounds = 10
   const generations = 1000

   for size in sizes
    for connection_p in connection_ps
      @sync @parallel for replication in 1:100
        run_network(size, generations, rounds, 3, x->connect_random(x, connection_p), string(connection_p), replication)
      end
    end
  end

  output = open("csv/sim3_ed.csv", "w")
  cols_ed = "id, type, a, b\n"
  write(output, cols_ed)
  for file_name in readdir("tmp/net/")
      file = open("tmp/net/" * file_name)
      for line in readlines(file)
          write(output, line * "\n")
      end
      close(file)
  end
  close(output)
  for file_name in readdir("tmp/net/")
      run(`rm tmp/net/$file_name`)
  end

  output = open("csv/sim3.csv", "w")
  cols = "id, typo, size, gen, agent, cons, var, signal, level\n"
  write(output, cols)
  for file_name in readdir("tmp/signal/")
      file = open("tmp/signal/" * file_name)
      for line in readlines(file)
          write(output, line * "\n")
      end
      close(file)
  end
  close(output)
  for file_name in readdir("tmp/signal")
      run(`rm tmp/signal/$file_name`)
  end
end

function simulation4()
  const rounds = 10
  const generations = 1000
  const size = 25

  @sync @parallel for replication in 1:100
    run_network(size, generations, 10, 3, x->connect_h5n2(x), "hierarchical", replication)
  end

  @sync @parallel for replication in 1:100
    run_network(size, generations, 10, 3, x->connect_ba(x), "ba", replication)
  end

  @sync @parallel for replication in 1:100
    run_network(size, generations, 10, 3, x->connect_random(x, 1.0), "complete", replication)
  end

  @sync @parallel for replication in 1:100
    run_network(size, generations, 10, 3, x->connect_random(x, 2.3), "random", replication)
  end

  output = open("csv/sim4_ed.csv", "w")
  cols_ed = "id, type, a, b\n"
  write(output, cols_ed)
  for file_name in readdir("tmp/net/")
      file = open("tmp/net/" * file_name)
      for line in readlines(file)
          write(output, line * "\n")
      end
      close(file)
  end
  close(output)
  for file_name in readdir("tmp/net/")
      run(`rm tmp/net/$file_name`)
  end

  output = open("csv/sim4.csv", "w")
  cols = "id, typo, size, gen, agent, cons, var, signal, level\n"
  write(output, cols)
  for file_name in readdir("tmp/signal/")
      file = open("tmp/signal/" * file_name)
      for line in readlines(file)
          write(output, line * "\n")
      end
      close(file)
  end
  close(output)
  for file_name in readdir("tmp/signal")
      run(`rm tmp/signal/$file_name`)
  end
end

#the 125 agent version of Simulation 4
function simulation5_social()
  const rounds = 10
  const generations = 500
  const size = 125

  @sync @parallel for replication in 1:50
    run_network(size, generations, 10, 3, x->connect_h5n3(x), "hierarchical", replication)
  end

  @sync @parallel for replication in 1:50
    run_network(size, generations, 10, 3, x->connect_random(x, 1.0), "complete", replication)
  end


  output = open("csv/sim5_social_ed.csv", "w")
  cols_ed = "id, type, a, b\n"
  write(output, cols_ed)
  for file_name in readdir("tmp/net/")
      file = open("tmp/net/" * file_name)
      for line in readlines(file)
          write(output, line * "\n")
      end
      close(file)
  end
  close(output)
  for file_name in readdir("tmp/net/")
      run(`rm tmp/net/$file_name`)
  end

  output = open("csv/sim5_social.csv", "w")
  cols = "id, typo, size, gen, agent, cons, var, signal, level\n"
  write(output, cols)
  for file_name in readdir("tmp/signal/")
      file = open("tmp/signal/" * file_name)
      for line in readlines(file)
          write(output, line * "\n")
      end
      close(file)
  end
  close(output)
  for file_name in readdir("tmp/signal")
      run(`rm tmp/signal/$file_name`)
  end
end

#the 125 agent version of Simulation 3
function simulation5_transitivity()
   const connection_ps = [ 0.1, 0.2, 0.3, 0.4 ,0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
   const sizes = [125]
   const rounds = 10
   const generations = 500

   for size in sizes
    for connection_p in connection_ps
      @sync @parallel for replication in 1:50
        run_network(size, generations, rounds, 3, x->connect_random(x, connection_p), string(connection_p), replication)
      end
    end
  end

  output = open("csv/sim5_transitivity_ed.csv", "w")
  cols_ed = "id, type, a, b\n"
  write(output, cols_ed)
  for file_name in readdir("tmp/net/")
      file = open("tmp/net/" * file_name)
      for line in readlines(file)
          write(output, line * "\n")
      end
      close(file)
  end
  close(output)
  for file_name in readdir("tmp/net/")
      run(`rm tmp/net/$file_name`)
  end

  output = open("csv/sim5_transitivity.csv", "w")
  cols = "id, typo, size, gen, agent, cons, var, signal, level\n"
  write(output, cols)
  for file_name in readdir("tmp/signal/")
      file = open("tmp/signal/" * file_name)
      for line in readlines(file)
          write(output, line * "\n")
      end
      close(file)
  end
  close(output)
  for file_name in readdir("tmp/signal")
      run(`rm tmp/signal/$file_name`)
  end
end

simulation3()
simulation4()
simulation5_social()
simulation5_transitivity()
