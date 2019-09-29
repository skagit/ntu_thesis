Transmission ( network )  
  FOR EACH agent IN network 
    replace actives with result of intergenerational transger
    replaces passives with new actives
    remove all old experiences from history
  END
END

Intergenerational Tansger ( agent )
  chosen := store for selected signals
  FOR EACH meaning in language
    select the signal used by the most unique agents for the meaning
    in case of ties, choose amongst them at random
    add signal to chose
  END
  
  FOR EACH signal in chosen
    IF chance <= 0.01
      replace the realization with the next id number
      increase reanalysis by 1
    END
  END
  
  FOR EACH signal in chose
    IF reanalysis level >= 20
      replace signal orign, and realization with next ids for each
      set renalysis level to 0
    END
  END
  RETURN chosen
END
  
