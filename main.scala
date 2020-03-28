//Iterated process on scala interpreter to check if everything was working as expected

import gridworld.agent.Agent

val b = Agent("A",20)

b.QAlgo(1000,0.1,0.2)

b.showAGame

