{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Type (typecheck) where
import System.Random (StdGen, random, Random)
import Term
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad (forM_, forM)

data Context = Context
  { gen  :: StdGen
  , ctx  :: [(String, Type)]
  , tops :: [(String, TopFunc)] }

type CheckM a = StateT Context (Either String) a

err :: String -> CheckM a
err s = lift $ Left s

unify :: Type -> Type -> CheckM ()
unify t t' | t == t' = return ()
unify t t' = err $ show t ++ " and " ++ show t' ++ " are not unifyable"

choose :: Random a => CheckM a
choose =
  do
    c <- get
    let (choice, next) = random (gen c)
    put c{ gen = next }
    return choice

lookupVar :: String -> CheckM Type
lookupVar v =
  do
    c <- get
    case lookup v $ ctx c of
      Just t -> return t
      Nothing ->
        case lookup v $ tops c of
          Just (TopFunc{args, ret}) -> return $ TFunc (map snd args) ret
          Nothing -> err $ "variable " ++ v ++ " not found"

setVar :: String -> Type -> CheckM ()
setVar v t =
  do
    c <- get
    put c{ctx = (v, t) : ctx c}

local :: CheckM a -> CheckM a
local e =
  do
    c <- get
    r <- e
    c' <- get
    put c'{ctx = ctx c}
    return r

checkExpr :: Expr -> CheckM (Expr, Type)
checkExpr (EInt i) = return (EInt i, TInt)
checkExpr (EUOp o e) =
  do
    (e', t) <- checkExpr e
    unify t TInt
    return (EUOp o e', TInt)
checkExpr (EBOp o a b) =
  do
    (a', t) <- checkExpr a
    unify t TInt
    (b', t') <- checkExpr b
    unify t' TInt
    return (EBOp o a' b', TInt)
checkExpr (EChoose a b) =
  do
    (a', ta) <- checkExpr a
    (b', tb) <- checkExpr b
    if ta == tb then
      return (EChoose a' b', ta)
    else do
      choice <- choose
      if choice then
        return (a', ta)
      else return (b', tb)
checkExpr (EAgg es) =
  do
    ets <- mapM checkExpr es
    let (es', ts) = unzip ets
    return (EAgg es', TAgg ts)
checkExpr (EProj e i) =
  do
    (e', t) <- checkExpr e
    case t of
      TAgg ts ->
        if 0 < i && i < length ts then
          return (EProj e' i, ts !! i)
        else
          err $ show t ++ " does not have enough entries to project #" ++ show i
      _ -> err $ "cannot get the projection of a " ++ show t
checkExpr (EList []) = err "TODO: empty list type checking"
checkExpr (EList es@(_:_)) =
  do
    ets <- mapM checkExpr es
    let (es', ts) = unzip ets
    forM_ ts $ unify $ head ts
    return (EList es', TList $ head ts)
checkExpr (EIndex e i) =
  do
    (e', t) <- checkExpr e
    t' <- case t of
      TList t' -> return t'
      _ -> err $ show t ++ " is not a list"
    (i', t'') <- checkExpr i
    unify t'' TInt
    return (EIndex e' i', t')
checkExpr (EVar v) = (EVar v,) <$> lookupVar v
checkExpr (ECall f args) =
  do
    (f', t) <- checkExpr f
    case t of
      TFunc tas tr ->
        if length tas /= length args then
          err $ show t ++ " does not have " ++
            show (length args) ++ " arguments"
        else do
          args' <- forM (zip args tas) $ \(a, t') -> do
            (a', ta) <- checkExpr a
            unify t' ta
            return a'
          return (ECall f' args', tr)
      _ -> err $ show t ++ " is not a function"


checkStat :: Stat -> CheckM (Stat, Type)
checkStat (SReturn Nothing) = return (SReturn Nothing, TUnit)
checkStat (SReturn (Just e)) =
  do
    (e', t) <- checkExpr e
    return (SReturn (Just e'), t)
checkStat (SIf b h e) =
  do
    (b', tb) <- checkExpr b
    unify tb TInt
    hful <- local $ mapM checkStat h
    let (h', th) = (map fst hful, snd $ last hful)
    eful <- local $ mapM checkStat e
    let (e', te) = (map fst eful, snd $ last eful)
    if th == te then
      return (SIf b' h' e', th)
    else do
      choice <- choose
      if choice then
        return (SBlock h', th)
      else return (SBlock e', te)
checkStat (SWhile b s) =
  do
    (b', tb) <- checkExpr b
    unify tb TInt
    sful <- local $ mapM checkStat s
    let (s', ts) = (map fst sful, snd $ last sful)
    return (SWhile b' s', ts)
checkStat (SChoose a b) =
  do
    aful <- local $ mapM checkStat a
    let (a', ta) = (map fst aful, snd $ last aful)
    bful <- local $ mapM checkStat b
    let (b', tb) = (map fst bful, snd $ last bful)
    if ta == tb then
      return (SChoose a' b', ta)
    else do
      choice <- choose
      if choice then
        return (SBlock a', ta)
      else return (SBlock b', tb)
checkStat (SBlock s) =
  do
    sful <- mapM checkStat s
    return (SBlock $ map fst sful, snd $ last sful)
checkStat (SCall f args) =
  do
    (e, t) <- checkExpr (ECall f args)
    case e of
      ECall f' args' -> return (SCall f' args', t)
      _ -> error "unreachable"
checkStat (SSet v e) =
  do
    (e', t) <- checkExpr e
    setVar v t
    return (SSet v e', t)

checkTop :: TopFunc -> CheckM TopFunc
checkTop (TopFunc{..}) = local $ do
  forM_ args $ uncurry setVar
  sful <- local $ mapM checkStat stats
  let (stats', t) = (map fst sful, snd $ last sful)
  unify t ret
  c <- get
  let (tl, tr) = break ((==name) . fst) $ tops c
  let top' = TopFunc {
      name = name
    , args = args
    , ret = ret
    , stats = stats' }
  put c { tops = tl ++ (name, top') : drop 1 tr }
  return top'

typecheck :: StdGen -> [TopFunc] -> Either String [TopFunc]
typecheck gen tops = evalStateT (mapM checkTop tops) ctx
  where ctx :: Context
        ctx = Context
          { gen = gen
          , ctx = []
          , tops = map (\x@(TopFunc { name }) -> (name, x)) tops }
