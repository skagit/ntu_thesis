The following is a sample of a document describing the various Marr’s levels of the high-level model. It is provided as a template for how to document the decisions involved in producing a computational model. In order to keep this document short, the social network side of the simulation has been omitted, as well as the lower level technical details, i.e. formula and parameter settings.

#Computational

The purpose of the high-level model is to capture the capacity of a social network to support morphological change. The processes of grammaticalization that lead to morphological complexity are laid out in Lou-Magnuson (2018) and the model is designed to track the frequency and depth to which these processes are able occur. It should be noted that the purpose of the model is not to measure morphological complexity, but rather the ability of a network to support the necessary conditions required for its development.

The main computational unit of the model, serving as both input and output, is a holistic signal object. These signals consist of several integer values that represent semantic content, phonological content, as well as the number off occurrences of grammaticalization processes that the signal has undergone. The transformation process is one of incremental change, whereby these signals are exchanged between agents, and altered or removed from the language through a simulation of intergenerational transfer. The signals used by agents can be measured at each generation to see how grammaticalization processes they have undergone, as well as the number of generations a signal survived.

#Algorithmic

As described above, the agents in the model exchange signals for a set period of time, followed by an intergenerational transfer procedure. Each agent has two collections of signals it keeps: one *active* collection of signals it uses for production, and another *passive* collection of all signals it has heard and understood. An agent is able to understand a signal provide that the phonological and meaning values are within a set distance from at least one signal in its passive collection; if something said is close enough to something the agent already heard, communication is successful. Every time an agent understands a signal, it has a set chance to add the signal to its active collection if not there already. Thus through repeated exposure agents can begin to produce signals learned from others.

During this exchange period, agents select a random partner according to their position within a social network graph, select a random meaning from a set number of meanings, and then randomly choose a signal that represents that meaning from their active collection. If the agent’s partner cannot understand the signal, another random signal with the same meaning is chosen from the active collection. If none of the active signals can be understood by the agent’s partner, a new signal with random phonological content and no grammaticalization events is created and added to each agent’s active collection. The idea here is that each agent preserves the ability to communicate with those in its social network, inventing new expressions for concepts in order to accommodate its neighbors.


Once a set number of communications has completed, each agent is replaced by a new agent that utilizes only the history of signals encountered by the agent it replaces; the model is usage based. During this transfer, first, one signal is selected for each meaning in the language. Then, these selected signals undergo a probabilistic process which may alter the integer values representing phonological content and grammaticalization events. The probabilities are based on the distribution of signals encountered by the agent being replaced. In addition to altering the values that abstract linguistic content and events, a signal may also be replaced by a new signal entirely. The idea here is that the input a language learner receives biases the changes seen through successive generations, such as phonological reductions to frequent items, or loss of forms that are rare or difficult to understand.

Once this intergenerational transfer is concluded for each agent, the signals selected by each agent are recorded, providing a record of the language for a given generation. The exchange portion of the simulation allows for altered or new signals to potentially spread across the social network and be adopted by others, while the transfer portion of the simulation accounts for the grammaticalization processes that are theorized to drive morphological change. Over hundreds and thousands of generations, the average number grammaticalization events per signal can be calculated, as well as individual histories of signals through both time (generations) and space (locations of speakers in the network).

#Implementational

The simulation was programmed in the Julia language, with statistical and network analysis conducted in R. Parameter settings and network implementation details can be found inside the main.jl file itself in functions named for each topology. Instructions to reproduce the simulations are found in the README.md file inside the directory of each model.

The signal exchange and intergeneration transfer processes have been described in plain English pseudo code with names that corresponds to actual variable and function names used in the simulation code. The signal exchange process is found in the diffusion.txt file, while the intergenerational process is found in the transmission.txt file.




