# a meaning is physically the same as a signal, but implements mulitset semantics
const Meaning = Vector{String}

# the meaning with no sememes
const EmptyMeaning = Meaning([])

# determine if the elements of another meaning are subset of the other as a multiset
function aresubset(els::Meaning, base::Meaning)
    base_cpy = deepcopy(base)

    for el in els
        match = false
        for i in 1:length(base_cpy)
            if el == base_cpy[i]
                base_cpy[i] = ""
                match = true
                break
            end
        end
        if !(match)
            return false
        end
    end
    return true
end

# determine if two multisets are identical
function aresame(a::Meaning, b::Meaning)
    return length(a) == length(b) && issubset(a, b)
end

# add a sememe to the multiset
function add!(base::Meaning, el::String)
    push!(base, el)
end

# add the contents of one meaning to another as a multiset
function add!(base::Meaning, els::Meaning)
    for el in els
        add!(base, el)
    end
end

# remove a sememe from the multiset
function remove!(base::Meaning, el::String)
    for i in 1:length(base)
        if base[i] == el
            return splice!(base, i)
        end
    end
end

# remove all the sememes of one multiset from another
function remove!(base::Meaning, els::Meaning)
    for el in els
        remove!(base, el)
    end
end

# # remove all the sememes of one multiset from another non-destructively
function meandiff(base::Meaning, subs::Meaning)
    base_cpy = deepcopy(base)
    remove!(base_cpy, subs)
    return base_cpy
end
