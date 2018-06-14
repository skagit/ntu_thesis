const Edge = Tuple{Int64, Int64}

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

#create either the 25 or 125 agent Hierarchical connections
function hier(n_agents)

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

    ties = n_agents == 25 ? connect_h5n2() : connect_h5n3()
end
