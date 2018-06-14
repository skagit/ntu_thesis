# a signal is an array of signs
const Signal = Vector{Sign}

# the empty signal
const EmptySignal = Signal([])

# given a base and signal that might occur in the base, find the start and end indicies where it occurs
# if the signal is not in the base, return zeros to indicate nonpresence
function findeyes(base::Signal, signal::Signal)
    offset = length(signal) - 1
    for i in 1:(length(base) - length(signal)) + 1
        if hash(base[i : i + offset]) == hash(signal)
            return i, i + offset
        end
    end
    return 0, 0
end

#given a sign, split the base into the prefix and suffix around it
function split(base::Signal, sign::Sign)
  for (i, val) in enumerate(base)
    if val == sign
      #prefix, sign, suffix
      return base[1:i-1], sign, base[i+1:end]
    end
  end

  #cannot find sign, return empty
  return EmptySignal, EmptySignal, EmptySignal
end

#given a signal, split the base into the prefix and suffix around it
function split(base::Signal, signal::Signal)
  (left_i, right_i) = findeyes(base, signal)
  if left_i != 0 #julia is 1-indexed, so an invalid 0-index indicates failure
    #prefix, signal, suffix
    return base[1:left_i - 1], signal, base[right_i + 1:end]
  else
    #cannot find signal
    return EmptySignal, EmptySignal, EmptySignal
  end
end
