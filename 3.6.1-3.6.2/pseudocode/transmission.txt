Transmission ( agent )
  derivations := store for derivations
  active := active repertoire of meaning-signal pairs

  FOR EACH stimuli IN language
    have the agent use their rewrite rules to produce a unique signal for stimuli
    store the derivation of signal in derivations
    add signal to active for that stimuli
  END

  FOR EACH segment IN rewrite rules
    count segment symbol in derivation
    IF chance equal to 0.001 times the count
      shorten the segment by one phoneme from end
      reproduce any rules in active that used the segment
    END
  END

  IF chance equal to 0.1
    randomly select rule from actives
    randomly remove one sememe from rule if more than one exists
  END

  replace with new random signal any signal in actives that have eroded away
  set passive repertoire to be a copy of active
  remove previous input history
  RETURN agent
END
