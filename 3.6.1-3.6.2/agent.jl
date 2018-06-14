#an agent has an ARS grammar, a collection of ARS start rules accumulated during diffusion, and active set of meaning-signal pairs (says), a passive set (knows) and set of productions and derivations to cache information during transmission.
mutable struct Agent
    grammar :: Grammar
    input :: Grammar
    pfs :: Vector{PF}
    says :: Dict{Meaning, Grammar}
    knows :: Dict{Meaning, Grammar}
end

#to make an agent you initialize it with empty data collections of the right type
function init_agent()
    return Agent(Grammar([]), Grammar([]), [], Dict{Meaning, Grammar}(), Dict{Meaning, Grammar}())
end
