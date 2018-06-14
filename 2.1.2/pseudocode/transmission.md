Transmission ( network )
  changes := store for updated innovation status
  FOR EACH agent IN network 
    add updated status to changes
  END
  
  FOR EACH each change and index in changes
    set the corresponding agent at that index to change
  END
END

Update Status ( agent )
  IF chance <= number of agent's connections that have innovated
    RETURN innovate
  END
  RETURN not innovated
END
