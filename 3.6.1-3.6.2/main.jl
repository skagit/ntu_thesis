@everywhere include("sign.jl")
@everywhere include("signal.jl")
@everywhere include("meaning.jl")
@everywhere include("rule.jl")
@everywhere include("grammar.jl")
@everywhere include("segment.jl")
@everywhere include("world.jl")
@everywhere include("grids.jl")
@everywhere include("agent.jl")
@everywhere include("edge.jl")
@everywhere include("analyze.jl")
@everywhere include("network.jl")

#run all of the network conditions for simulations 6 and 7

function simulation6()
    const connection_ps = [ 0.1 , 0.2, 0.3, 0.4 ,0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
    const sizes = [25, 125]
    const rounds = 10
    const generations = 200
    connectivity = []

    stims = kirby_stims()
    w = init_world(10, (1,5), 25, (2,8), stims)
    grammar3 = holistic_grammar(stims, w)
    grammar4 = [Rule("S",r.meaning, r.signal) for r in grammar3[2:end]]
    init_g = segment(grammar4)

    for size in sizes
        for connection_p in connection_ps
            @sync @parallel for replication in 1:50

                if connection_p == "hier"
                    network = init_hier_network(connection_p,size, w)
                else
                    network = init_pcon_network(connection_p, size, w)
                end
                network.id = replication
                push!(connectivity, ((network.topology, network.size, network.id), network.ties))

                for agent in network.agents
                    agent.grammar = deepcopy(init_g)
                end

                println(join([network.topology, network.size, network.id], "~~~~~~~~~~~~~~"))

                run!(network, generations, rounds)
            end
        end
    end

    const header = "topology, size, replication , generation, ms, ws, synthesis\n"

    output = open("csv/sim6.csv", "w")
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
    output = open("csv/sim6_ed.csv", "w")
    write(output, con_header)

    for ((topology, size, id), ties) in connectivity
        for (a,b) in ties
            line = join([topology, size, id, a, b], ",")
            write(output, line * "\n")
        end
    end
    close(output)
end

function simulation7()
    const connection_ps = [0.4, 1.0, "hier"]
    const sizes = [25, 125]
    const rounds = 10
    const generations = 200
    connectivity = []

    stims = kirby_stims()
    w = init_world(10, (1,5), 25, (2,8), stims)

    grammar3 = holistic_grammar(stims, w)
    grammar4 = [Rule("S",r.meaning, r.signal) for r in grammar3[2:end]]
    init_g = segment(grammar4)

    for size in sizes
        for connection_p in connection_ps
            @sync @parallel for replication in 1:50

                if connection_p == "hier"
                    network = init_hier_network(connection_p,size, w)
                else
                    network = init_pcon_network(connection_p, size, w)
                end
                network.id = replication
                push!(connectivity, ((network.topology, network.size, network.id), network.ties))

                for agent in network.agents
                    agent.grammar = deepcopy(init_g)
                end

                println(join([network.topology, network.size, network.id], "~~~~~~~~~~~~~~"))

                run!(network, generations, rounds)
            end
        end
    end

    const header = "topology, size, replication , generation, ms, ws, synthesis\n"

    output = open("csv/sim7.csv", "w")
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
    output = open("csv/sim7_ed.csv", "w")
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
