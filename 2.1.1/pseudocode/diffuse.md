Communicate Round ( network )
  randomize network's connections
  FOR EACH connection
    diffuse the two agents
  END
END

Diffuse (agent_a, agent_b)
  IF one agent has innovated and the other has not
    IF chance <= 0.1
      set both agents to have innovated
    END
  END
END
