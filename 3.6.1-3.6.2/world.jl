#a world has a list of sememe and phonemes, as well as min-max limits on how long a meaning or signal can be in the language, and finally a list of cannonical stimuli for the agents to converse with
mutable struct World
  sememes :: Signal
  slims :: Tuple{Int64,Int64}
  phonemes :: Signal
  plims :: Tuple{Int64,Int64}
  stimuli :: Vector{Meaning}
end

#create a random signal using the paramaters of the language
function generate_signal(world)
  (min, max) = world.plims
  len = rand(min:max)
  return Signal([ rand(world.phonemes) for _ in 1:len])
end

#create a meaning using the parameters of the language
function generate_meaning(world)
  (min, max) = world.slims
  len = rand(min:max)
  meaning = Meaning([])
  count = 0
  while length(meaning) < len && count < len * 2
    next = rand(world.sememes)
    if !(next in meaning)
      push!(meaning, rand(world.sememes))
    end
    count += 1
  end
  return meaning
end

# create a new world object
function init_world(sememes, slims, phonemes, plims, stimuli = [])
  sememes = [ "s"*string(i) for i in 1:sememes]
  phonemes = [ "p"*string(i) for i in 1:phonemes]
  return World(sememes, slims, phonemes, plims, stimuli)
end

# create the simon kirby inspired stimuli from his 2001 paper
function kirby_stims()
  as = [ "a"*string(i) for i in 1:5]
  bs = [ "b"*string(i) for i in 1:5]
  stimuli = []
  for a in as
    for b in bs
      push!(stimuli, Meaning([a,b]))
    end
  end
  return stimuli
end

# create an ARS for as set of stimuli where each stimuli is a word
function holistic_grammar(stims, world)
  sr = StoR("S", "SG0", "SG0")
  segs = []
  seen = []
  for stim in stims
    next_sig = generate_signal(world)
    while next_sig in seen
      next_sig = generate_signal(world)
    end
    seg = Rule("SG0", Meaning(stim), Signal(next_sig))
    push!(seen, next_sig)
    push!(segs, seg)
  end
  return Grammar([[sr];segs])
end
