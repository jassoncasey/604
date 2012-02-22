type TypeContext = [(Ast.Name,TypeSymbol)]

type ConstraintSet = [(TypeSymbol,TypeSymbol)]

type Substitution = [(TypeSymbol,TypeSymbol)]

lookUp :: TypeContext -> String -> Maybe TypeSymbol
lookUp (((Ast.Identifier nextSym),ty):cs) sym
  | sym == nextSym = Just ty
  | otherwise = lookUp cs sym
lookUp (((Ast.Unique _),ty):cs) sym = lookUp cs sym
lookUp _ _ = Nothing

bindVar :: TypeContext -> String -> TypeSymbol -> TypeContext
bindVar context sym ty = ((Ast.Identifier sym),ty):context