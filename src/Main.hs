{-# LANGUAGE TemplateHaskell #-}
module Main where
       
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe
import           Control.Lens
import           Debug.Trace
       
data Term = Term String 
  deriving (Show,
  Eq, Ord)

data Statement = 
  StatementInherit 
    { stmtSubject :: Term
    , stmtPredicate :: Term
    }
  deriving (Eq, Ord)
  
instance Show Statement where
  show (StatementInherit term1 term2) = show term1 ++ " -> " ++ show term2
   
type Vocabulary = Set.Set Term
type Extension = Set.Set Term
type Intension = Set.Set Term
  
data Knowledge = Knowledge
  { _kStatements :: Set.Set Statement
  , _kExtensions :: Map.Map Term Extension
  , _kIntensions :: Map.Map Term Intension
  } deriving (Show)
makeLenses ''Knowledge
  
intensions :: Term -> Set.Set Statement -> [Statement]
intensions term stmts = [st | st@(StatementInherit _ p) <- Set.toList stmts, p == term]

extensions :: Term -> Set.Set Statement -> [Statement]
extensions term stmts = [st | st@(StatementInherit s _) <- Set.toList stmts, s == term]
           
queryIntensions :: Term -> Knowledge -> [Statement]
queryIntensions t k = intensions t (k^.kStatements)

queryExtensions :: Term -> Knowledge -> [Statement]
queryExtensions t k = extensions t (k^.kStatements)
                
newStmt :: String -> String -> Statement
newStmt left right = StatementInherit (Term left) (Term right)
 
newKnowledge :: Set.Set Statement -> Knowledge 
newKnowledge stmts = Knowledge stmts Map.empty Map.empty
  
transHull :: Knowledge -> Knowledge           
transHull knowledge =
  let 
    step = derive knowledge
    newFullKnowledge = newKnowledge $ Set.union (knowledge^.kStatements) step
  in if Set.size step == 0 then newFullKnowledge
    else traceShow (step) $ transHull newFullKnowledge
  
stmtExists :: Knowledge -> Statement -> Bool
stmtExists knowledge s = Set.member s (knowledge^.kStatements)
    
-- | derive all statements derivable from the current knowledge
derive :: Knowledge -> Set.Set Statement
derive knowledge = Set.fromList uniqueDeductions
  where
    stmts = Set.toList (knowledge^.kStatements)
    crossStatements = [(s1, s2) | s1 <- stmts, s2 <- stmts]
    deductions = catMaybes $ map (uncurry deduce) crossStatements
    uniqueDeductions = filter (not . stmtExists knowledge) deductions

deduce :: Statement -> Statement -> Maybe Statement
deduce (StatementInherit m1 p) (StatementInherit s m2) | m1 == m2 && s /= p = Just (StatementInherit s p)
deduce (StatementInherit p m1) (StatementInherit m2 s) | m1 == m2 && s /= p = Just (StatementInherit p s)
deduce _ _ = Nothing
       
knowledge = newKnowledge $ Set.fromList [newStmt "robin" "bird", newStmt "bird" "animal", newStmt "water" "liquid"]

main :: IO ()
main = do
  let knowledge = newKnowledge $ Set.fromList [newStmt "robin" "bird", newStmt "bird" "animal", newStmt "water" "liquid"]
  print $ transHull knowledge
