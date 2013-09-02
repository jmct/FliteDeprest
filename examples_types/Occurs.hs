
type Prog     = [Template]
type Template = (Arity,App,[App])
type Arity    = Int
type App      = [Atom]
data Atom =
        FUN Arity Int   -- Function with arity and address
      | ARG Int         -- Reference to a function argument
      | PTR Int         -- Pointer to an aplication
      | CON Arity Int   -- Constructor with arity and index
      | INT Int         -- Integer literal
      | PRI String      -- Primitive function name
      | TAB Int         -- Case table
     deriving Show


  
tri5=[ (0,[FUN 1 1, INT 5,TYPE [TCONS "Int" []]],[])
      ,(1,[INT 1, PTR 0, TAB 2, ARG 0,TYPE [TCONS "Int" []]],     
          [[ARG 0, PRI "(<=)"]])
      ,(2,[ARG 1, PTR 0,TYPE [TCONS "Int" []]],
          [[FUN 1 1, PTR 1, PRI "(+)"],
           [INT 1,PTR 2],
           [ARG 1, PRI "(-)"]])
      ,(2,[INT 1,TYPE [TCONS "Int" []]],[])
      ]

