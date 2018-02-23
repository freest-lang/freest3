module Main where

import Terms.Parser
import qualified Data.Map.Strict as Map
import TypeChecking.TypeChecking


main :: IO ()
main = do
  putStrLn "Starting parser ...\n"
  prelude <- mainProgram "src/Terms/prelude.hs" Map.empty
  (prelude, _) <- case prelude of
               Left err -> do{ putStr (show(err)); return $ error ""}
               Right d  -> return d

  -- putStrLn "\n"
  -- print $ prelude
  -- putStrLn "\n"
  prog     <- mainProgram "src/Terms/test.hs" prelude
  (p1,p2)  <- case prog of
                Left err -> do{ putStr (show(err)); return $ error ""}
                Right d  -> return d

  -- putStrLn "\n"
  -- print p1

  putStrLn "No parser errors found... \n"
  putStrLn "TypeChecking...\n"
  a <- pure $ Map.mapWithKey (\fun (a,e) -> typeCheck a e fun p1) p2
  mapM (>>= putStrLn . show) a
  return ()




  -- test
  --   :: Applicative f =>
  --      Terms.Terms.VarEnv
  --      -> Map.Map k (t, Terms.Terms.Expression)
  --      -> f (Map.Map k (IO Types.Types.Type))


-- typeCheck :: Expression -> VarEnv -> IO(Type)
