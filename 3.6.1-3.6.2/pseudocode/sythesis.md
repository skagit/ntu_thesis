Analyze ( ARS )
  RDG := create rewrite dependency graph from ARS rules
  words := store for word-class symbols
  syntax := store for syntactic transformation symbols
  notsure := store for yet to be analyzed symbols
  
  begin by collecting all symbols in graph pointed to by start symbol
  add them to notsure
  WHILE not empty notsure
    IF symbol does not violates constraints OR symbol is segment, add to words
    ELSE add symbol to syntax and add each symbol it can rewrite to to notsure
    END
  END
  RETURN words and syntx
END

Violates Constraints Against Word ( symbol )
  IF symbol is recursive OR outside rewriteable
    RETURN true
  END
  RETURN false
END

Is Recursive ( symbol )
  IF endge in graph points to symbol from symbol
    RETURN true
  END
  RETURN false
END

Is Outside Rewriteable ( symbol, parent )
  IF edge in graph that points to symbol from location other than parent
    RETURN true
  END
  RETURN false
END
  
