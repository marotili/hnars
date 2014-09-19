{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import FingerTree       
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe
import           Control.Lens
import           Debug.Trace
import           Data.Monoid
                 
import           Data.PSQueue
       
import           Control.Concurrent
import           PrioritySync.PrioritySync

data Term = Term String 
  deriving (Show,
  Eq, Ord)

data Statement = 
  StatementInherit 
    { stmtSubject :: Term
    , stmtPredicate :: Term
    }
  deriving (Eq, Ord)

  
newtype Frequency = Frequency { unFreq :: Float }
  deriving (Show)
newtype Confidence = Confidence { unConf :: Float }
  deriving (Show)

data Judgement = Judgement Statement Frequency Confidence
  deriving (Show)
  
data Question = 
  QuestionStmt Statement
  | QuestionSubject Term
  | QuestionPredicate Term
  deriving (Show)
  
data Sentence = 
  SentenceJudgement Judgement
  | SentenceQuestion Question
  deriving (Show)
  
instance Show Statement where
  show (StatementInherit term1 term2) = show term1 ++ " -> " ++ show term2
   
type Vocabulary = Set.Set Term
type Extension = Set.Set Term
type Intension = Set.Set Term
     
data Concept = Concept
  { _cJudgements :: Set.Set Judgement
  , _cQuestions :: Set.Set Question
  , _cExtension :: Extension
  , _cIntension :: Intension
  , _cTerm :: Term
  , _cPriority :: Float
  }

makeLenses ''Concept

instance Measured Max Concept where
  measure c = Max (c^.cPriority) 0 (c^.cPriority)
  
instance Eq Concept where
  c1 == c2 = (c1^.cTerm) == (c2^.cTerm)
  
instance Ord Concept where
  c1 <= c2 = (c1^.cTerm) <= (c2^.cTerm)
  
data Knowledge = Knowledge
  { _kStatements :: Set.Set Judgement
  , _kExtensions :: Map.Map Term Extension
  , _kIntensions :: Map.Map Term Intension
  } deriving (Show)
makeLenses ''Knowledge
           
intensions :: Term -> Set.Set Statement -> Set.Set Statement
intensions term stmts = Set.fromList [st | st@(StatementInherit _ p) <- Set.toList stmts, p == term]

extensions :: Term -> Set.Set Statement -> Set.Set Statement
extensions term stmts = Set.fromList [st | st@(StatementInherit s _) <- Set.toList stmts, s == term]
           
-- queryIntensions :: Term -> Knowledge -> [Statement]
-- queryIntensions t k = intensions t (k^.kStatements)

-- queryExtensions :: Term -> Knowledge -> [Statement]
-- queryExtensions t k = extensions t (k^.kStatements)
                
newStmt :: String -> String -> Statement
newStmt left right = StatementInherit (Term left) (Term right)
 
newKnowledge :: Set.Set Judgement -> Knowledge 
newKnowledge stmts = Knowledge stmts Map.empty Map.empty
             
-- positiveEvidence :: Statement -> Knowledge -> Int
-- positiveEvidence (StatementInherit s p) k = 
--     Set.size (Set.intersection (extensions s stmts) (extensions p stmts))
--   + Set.size (Set.intersection (intensions s stmts) (intensions p stmts))
--   where
--     stmts = k^.kStatements
  
-- negativeEvidence :: Statement -> Knowledge -> Int
-- negativeEvidence (StatementInherit s p) k =
--     Set.size ((extensions s stmts) Set.\\ (extensions p stmts))
--   + Set.size ((intensions p stmts) Set.\\ (intensions s stmts))
--   where
--     stmts = k^.kStatements

-- evidence :: Statement -> Knowledge -> Int 
-- evidence s k = positiveEvidence s k + negativeEvidence s k
         
-- frequency :: Statement -> Knowledge -> Float
-- frequency s k = (fromIntegral $ positiveEvidence s k) / (fromIntegral $ evidence s k)
   
-- horizont :: Float       
-- horizont = 1

-- confidence :: Statement -> Knowledge -> Float
-- confidence s k = 1 - horizont / (horizont + fromIntegral (evidence s k))
           
-- lowerFreq :: Statement -> Knowledge -> Float
-- lowerFreq s k = fromIntegral (positiveEvidence s k) / (fromIntegral (evidence s k) + horizont)

-- upperFreq :: Statement -> Knowledge -> Float
-- upperFreq s k = (horizont + fromIntegral (positiveEvidence s k)) / (fromIntegral (evidence s k) + horizont)
        
-- transHull :: Knowledge -> Knowledge           
-- transHull knowledge =
--   let 
--     step = derive knowledge
--     newFullKnowledge = newKnowledge $ Set.union (knowledge^.kStatements) step
--   in if Set.size step == 0 then newFullKnowledge
--     else traceShow (step) $ transHull newFullKnowledge
  
-- stmtExists :: Knowledge -> Statement -> Bool
-- stmtExists knowledge s = Set.member s (knowledge^.kStatements)
    
-- -- | derive all statements derivable from the current knowledge
-- derive :: Knowledge -> Set.Set Statement
-- derive knowledge = Set.fromList uniqueDeductions
--   where
--     stmts = Set.toList (knowledge^.kStatements)
--     crossStatements = [(s1, s2) | s1 <- stmts, s2 <- stmts]
--     deductions = catMaybes $ map (uncurry deduce) crossStatements
--     uniqueDeductions = filter (not . stmtExists knowledge) deductions

-- deduce :: Statement -> Statement -> Maybe Statement
-- deduce (StatementInherit m1 p) (StatementInherit s m2) | m1 == m2 && s /= p = Just (StatementInherit s p)
-- deduce (StatementInherit p m1) (StatementInherit m2 s) | m1 == m2 && s /= p = Just (StatementInherit p s)
-- deduce _ _ = Nothing
       
-- -- knowledge = newKnowledge $ Set.fromList [newStmt "robin" "bird", newStmt "bird" "animal", newStmt "water" "liquid"]

-- main :: IO ()
-- main = do
--   let knowledge = newKnowledge $ Set.fromList [newStmt "robin" "bird", newStmt "bird" "animal", newStmt "water" "liquid"]
--   print $ transHull knowledge

type Priority = Float

data Bag k a = Bag
  { _prioQueue :: PrioQueue a
  , _keyHash :: Map.Map k a
  }
  
type ConceptBag = Bag Term Concept 
type JudgementBag = Bag Term Judgement
     
main = do
  return () 
