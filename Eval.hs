

import AST

-- Delta rules for standard arithmetic operations covered by the SPU
deltaArith :: Expr -> Int
deltaArith (Binary _ (