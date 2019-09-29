GRIDS ( rewrite_rules )
  WHILE can find new abstract pattern
    update rewrite_rules with pattern
    search for additional patterns in updated rewrite_rules
  END

  WHILE can find generalization
    update rewrite_rules with generalization
    search for additional generalizations in updated rewrite_rules
  END

  IF rewrite_rules was updated
    RETURN GRIDS ( updated rewrite_rules )
  END

  RETURN ( rewrite_rules )
END

Find Pattern ( rules )
  sequences := store for found patterns
  filter out rules that contain phoneme or sememe content
  search all rule signals for repeated sequences of length 2, and add to sequences
  WHILE found sequences of length n
    search for sequences of length n + 1
  END
  RETURN sequences
END

Find Generalization ( rules )
  non_starts := filter out rules that begin with start symbol
  non_segments := filter out rules that have sememe or phoneme content

  FOR EACH rule IN non_starts
    FOR EACH other_rule IN non_starts
      IF meaning and signal the rules match AND symbols of the rules are different
        RETURN symbols of both rules
      END
    END
  END

  FOR EACH rule IN non_segments
    FOR EACH other rule IN non_segments
      IF meanings differ by on sememe AND signals differ by one phoneme in place
        RETURN symbols the two symbols that differ
      END
    END
  END
END

Update Pattern ( rewrite_rules, patterns)
  sort patterns using MDL and retain top scoring pattern
  create a new rule with pattern symbols as meaning and signal
  replace each occurrence of pattern in rewrite_rules with symbol of new_rule
  RETURN combined updated rewrite_rules with new_rule that captures pattern
END

Update Generalization ( rewrite_rules, symbols )
  pick one symbol at random to be retained
  replace each occurrence of other symbol with retained symbol in rewrite_rules
  remove one of the now duplicate rules that were used to find the generalization
  RETURN updated rewrite_rules
END
