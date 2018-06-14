#simulation agents have either innovated or not
mutable struct Agent
    innovated :: Bool
end

#if both agents have innovated or not innovated, diffusion is impossible
function can_diffuse(agent_i, agent_j)
    return  (agent_i.innovated || agent_j.innovated) &&
           !(agent_i.innovated && agent_j.innovated)
end

#if diffusion has occured, make sure both agents are set to innovated
#only one will change, but it is faster to set both rather than check each and set the one who had not yet innovated
function diffuse!(agent_i, agent_j)
    if rand() <= 0.01
        agent_i.innovated = true
        agent_j.innovated = true
    end
end

#the network consists of the agents and a list of unidrected edges
struct Network
    agents :: Vector{Agent}
    ties :: Vector{Tuple{Int, Int}}
end

#to make a network you need the connection probability and the number of agents
#make the appropriate number of agents and randomly create the edges between them
function init_network(p, n_agents)
    agents = [Agent(false) for _ in 1:n_agents]
    ties = pcon(p, n_agents)
    return Network(agents, ties)
end

#this function was not used in the thesis results for this model
#it generates the 25 and 125 agent Hierarchical network ties
function init_network_h(n_agents)

    function connect_h5n2()
        base = append!([ (1,2) , (2,3) , (3,4) , (4,1) ] , [(i, 5) for i in 1:4])
        externs = [(x+(5*i), y+(5*i)) for i in 1:4 for (x, y) in base]
        centrals = [(x + (5*i), 5) for x in 1:4 for i in 1:4]
        return append!(append!(base, externs), centrals)
    end

    function connect_h5n3()
        h5n2s = [connect_h5n2() for i in 1:5]
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

    agents = [Agent(false) for _ in 1:n_agents]
    ties = n_agents == 25 ? connect_h5n2() : connect_h5n3()
    return Network(agents, ties)
end

#have a random agent switch their status
#used to being the simulations
function innovate!(network)
    rand(network.agents).innovated = true
end

#write out one line of data to the outfile in csv format
function record(network, outfile, id, round)
    # size, topology, replication, round, agent id, innovated
    for (i, agent) in enumerate(network.agents)
        (size, topology, replication) = split(id, "_")
        out_variables = [size, topology, replication, round, i, agent.innovated ? 1 : 0]
        out_string = join(out_variables, ",")
        write(outfile, out_string * "\n")
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

#to simulate a network enter the number of rounds and replication id number
#this function generates a csv for each replication so that multiprocessing is easier to implement
function run!(network, rounds, id)
    outfile = open( "./tmp/" * id * ".csv" , "w")
    innovate!(network)
    record(network, outfile, id, 0)
    for round in 1:rounds
        ties = shuffle(network.ties)
        for tie in ties
            agent_i = network.agents[tie[1]]
            agent_j = network.agents[tie[2]]
            if can_diffuse(agent_i, agent_j)
                diffuse!(agent_i, agent_j)
            end
        end
        record(network, outfile, id, round)
    end
    close(outfile)
end

#to reproduce the data set, for each network type and size, run 30 replications
#then compile the locally saved data files into two csv's, one with the ntwork structure and one with the simulation results
function main()

    const connection_ps = [ 0.1, 0.2, 0.3 ,0.4 ,0.5, 0.6, 0.7, 0.8, 0.9, 1.0, "hierarchical"]
    const sizes = [25, 125]
    const rounds = 100
    connectivity = []

    for size in sizes
        for connection_p in connection_ps
            for replication in 1:30
                if connection_p == "hierarchical"
                    network = init_network_h(size)
                else
                    network = init_network(connection_p, size)
                end
                id = join([size, connection_p, replication], "_")
                run!(network, rounds, id)
                println(id)
            end
        end
    end

    const header = "size, topology, replication, round, agent id, innovated\n"

    output = open("csv/sim1.csv", "w")
    write(output, header)
    for file_name in readdir("tmp")
        file = open("tmp/" * file_name)
        for line in readlines(file)
            write(output, line * "\n")
        end
        close(file)
    end
    close(output)
    for file_name in readdir("tmp")
        run(`rm tmp/$file_name`)
    end

    const con_header = "topology, size, replication , a, b\n"
    output = open("csv/sim1_ed.csv", "w")
    write(output, con_header)

    for ((topology, size, id), ties) in connectivity
        for (a,b) in ties
            line = join([topology, size, id, a, b], ",")
            write(output, line * "\n")
        end
    end
    close(output)

end

main()
