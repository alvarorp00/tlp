module Lib where

import qualified Data.Set as Set

-- EXERCISE 14. define functions boolChurch and boolUnchurch which translate between the Bool type and Church-encoded booleans.

boolChurch :: Bool -> (Bool -> Bool -> Bool)
boolChurch True  = \x y -> x
boolChurch False = \x y -> y

boolUnchurch :: (Bool -> Bool -> Bool) -> Bool
boolUnchurch f = f True False

-- EXERCISE 15. Analogously to the previous exercise, define functions intChurch and intUnchurch which translate between the int type and Church-encoded integers.

intChurch :: Integral a => Int -> (a -> a) -> a -> a
intChurch 0 = \f x -> x
intChurch n = \f x -> f (intChurch (n-1) f x)

intUnchurch :: Integral a => (a -> a) -> a -> Int
intUnchurch f x = length $ takeWhile (/= x) $ iterate f x


-- EXERCISE 16. Define a Haskell datatype to represent the abstract syntax of the λ-calculus

-- e :: x | (λx.e) | (e e')
data Term = Var String | App Term Term | Lambda String Term deriving (Eq)


-- EXERCISE 17. Based on the previous exercise, define a Haskell function that obtains the free variables of a lambda term.

freeVars :: Term -> Set.Set(String)
freeVars (Var x) = Set.singleton x
freeVars (App e e') = Set.union (freeVars e) (freeVars e')
freeVars (Lambda x e) = Set.delete x (freeVars e)


-- EXERCISE 18. Based on the previous exercises, define a Haskell function that implements capture avoiding substitution.
-- Ref. https://yangdanny97.github.io/blog/2019/05/25/capture-avoiding-substitution

-- This function performs explicit alpha-conversion to avoid capture
capAvoidSub :: Term -> String -> Term -> Term
capAvoidSub e x e' = capAvoidSub' e x e' (freeVars e')  -- fvs is the set of free variables of e' and it's more efficient to pass it as an argument
    where capAvoidSub' :: Term -> String -> Term -> Set.Set(String) -> Term
          capAvoidSub' e@(Var x) x' e' _ = if x == x' then e' else e
          capAvoidSub' e@(App e1 e2) x' e' fvs = App (capAvoidSub' e1 x' e' fvs) (capAvoidSub' e2 x' e' fvs)
          capAvoidSub' e@(Lambda x e1) x' e' fvs =
            if x == x' then -- x' is bound in e so we can't replace it with e'
                e
            else -- x' is not bound in e so we can safely replace it with e'
              if Set.member x fvs then -- x is a free variable of e' (which we are replacing x' with)
                  let x'' = freshVar x fvs in
                  Lambda x'' (capAvoidSub' (replaceFreeVars e1 x x'') x' e' fvs)
              else
                  Lambda x (capAvoidSub' e1 x' e' fvs)

-- Replace the free variable x' (first String) with x'' (second String) in _e (first Term)
replaceFreeVars :: Term -> String -> String -> Term
replaceFreeVars _e@(Var x) x' x'' = if x == x' then (Var x'') else (Var x)
replaceFreeVars _e@(App e e') x' x'' = App (replaceFreeVars e x' x'') (replaceFreeVars e' x' x'')
replaceFreeVars _e@(Lambda x e) x' x'' = if x == x' then _e else (Lambda x (replaceFreeVars e x' x''))  -- x' is not a free variable of e so we don't need to replace it

-- Find a fresh variable that is not in the set of free variables from a given variable x
freshVar :: String -> Set.Set(String) -> String
freshVar x fvs = if Set.member x fvs then freshVar (x ++ "'") fvs else x

-- EXERCISE 19. Based on the previous exercises, define a Haskell function that implements the β-reduction (one step).

betaReduce :: Term -> Term
betaReduce _e@(App e e') = betaReduce'
    where betaReduce' :: Term
          betaReduce' = case e of
              (Lambda x e1) -> capAvoidSub e1 x e'
              (App e1 e2) -> App (betaReduce (App e1 e2)) e'
              _ -> _e -- We can't reduce a variable
betaReduce _e = _e -- We can't reduce a variable or a lambda term by themselves

-- EXERCISE 20. Based on the previous exercises, define a Haskell function that reduces a lambda term into β-normal form when possible.

betaNormalForm :: Term -> Term
betaNormalForm e = if e == e' then e else betaNormalForm e'
    where e' = betaReduce e

-- STRFY. Stringify a lambda term.

strfy :: Term -> String
strfy (Var x) = x
strfy (App e e') = "(" ++ strfy e ++ " " ++ strfy e' ++ ")"
strfy (Lambda x e) = "(λ" ++ x ++ "." ++ strfy e ++ ")"

instance Show Term where
    show = strfy

-- TEST. Test the functions defined above.

mainLib :: IO()
mainLib = do
    let e = App (Lambda "x" (Var "x")) (Var "y")
    let e2 = Lambda "x" (Lambda "z" (App (App (Lambda "x" (Var "x")) (Var "y")) (Var "z")))
    let e3 = Lambda "y" (Var "y")
    let false = boolChurch False
    let true = boolChurch True
    let l_true = Lambda "x" (Lambda "y" (Var "x"))
    -- print $ e
    -- print $ e2
    -- print $ e3
    -- print $ freeVars e
    -- print $ replaceFreeVars e "x" "z"
    -- print $ capAvoidSub e "x" (Var "z")
    -- print $ capAvoidSub e "y" (Var "z")
    -- print $ capAvoidSub e2 "y" l_true
    -- print $ capAvoidSub (Var "y") "y" l_true
    let ___e = App ( App ( Lambda "x" (Lambda "y" (Lambda "z" (App (App (Var "x") (Var "y")) (Var "z")))) ) ( Lambda "x" ( App (Var "x") (Var "x") ) ) ) ( Lambda "x" (Var "x") )
    let ___x = Var "x"
    -- print $ ___e
    -- print $ betaReduce ___e ___x
    -- let __e = Lambda "y" (App (Var "y") (Var "z"))
    -- print $ capAvoidSub __e "z" (Lambda "y" (App (App (Var "x") (Var "y")) (Var "y")))
    let ____e = App (___e) (___x)
    print $ betaNormalForm ___e
