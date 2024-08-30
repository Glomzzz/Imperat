{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Imperat () where

import GHC.Base (IO (IO))

data Expr
  = List
  | Head Expr
  | Tail Expr
  deriving (Show)

data Program
  = Read [String]
  | Assign Expr
  | Do [Program]
  | While Expr Program
  | Write Expr
  | Print
  deriving (Show)

eval List vs = vs
eval (Head expr) vs = [head $ eval expr vs]
eval (Tail expr) vs = tail $ eval expr vs

run (Assign expr) vs (c : cs) = run c (eval expr vs) cs
run (Do (c : cs)) vs cs' = run c vs (cs ++ cs')
run w@(While cond cmd) vs cs' =
  if eval cond vs == ["nil"]
    then let (c : cs) = cs' in run c vs cs
    else run cmd vs (w : cs')
run Print vs (c : cs) = print vs >> run c vs cs
run (Write expr) vs [] = return $ eval expr vs

countP (Assign expr) vs' co (c : cs) = countP c (eval expr vs') (countE expr ("Assign" : co)) cs
countP (Do (c : cs)) vs co cs' = countP c vs co (cs ++ cs')
countP w@(While cond cmd) vs co cs' =
  if eval cond vs == ["nil"]
    then let (c : cs) = cs' in countP c vs cc cs
    else countP cmd vs cc (w : cs')
  where
    cc = countE cond ("While" : co)
countP Print vs co (c : cs) = countP c vs co cs
countP (Write _) _ co [] = co

countE List co = "Fetch" : co
countE (Head expr) co = countE expr $ "Head" : co
countE (Tail expr) co = countE expr $ "Tail" : co

count (Read vs : p : ps) = reverse $ countP p vs [] ps

interpret (Read vs : p : ps) = run p vs ps

test args =
  [ Read args,
    While (Head List) (Assign $ Tail List),
    Write List
  ]

pgms =
  [ Read ["a", "b", "nil", "c"],
    While
      (Head List)
      ( Do
          [ Print,
            Assign $ Tail List
          ]
      ),
    Write List
  ]

main = count $ test ["A", "nil", "B"]
