Communicate Round ( network )
  randomize network's connections
  FOR EACH connection
    diffuse the two agents, once each as the speaker
  END
END

Diffuse (speaker, hearer)
  pick a random meaning
  pick a random signal from speaker's actives for the meaning
  IF hearer can understand OR speaker can repair
    add speaker id and signal to hearers history
    add signal to passive if not already present
    IF signal already in passive AND chance <= 0.25
      add signal to heares active
    END
  ELSE
    create new zero level signal
    add signal to each agents active and passive
    add each agents id and signal to each other's history
  END
END

Can Understand Signal (signal, hearer)
  FOR EACH passive signal hearer has for meaning
    IF meanings and origins match && renalysis levels are within 1
      RETURN true
    END
  END
  RETURN false
END

Can Repair ( speaker, hearer, meaning )
  FOR EACH signal speaker has for the meaning
    IF the hearer can understand
      RETURN true
    END
  END
  RETURN false
END
