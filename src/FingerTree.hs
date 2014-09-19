{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module FingerTree where
       
--import           Control.Lens hiding ((<|))
import Data.Monoid       
import Debug.Trace
       
class Monoid v => Measured v a where
  measure :: a -> v
  
newtype Max = Max Float deriving (Show, Eq, Ord)
        
instance Monoid Max where
  mempty = Max 0
  mappend (Max a) (Max b) = Max (max a b)

data Node v a = Node2 v a a
              | Node3 v a a a
              deriving (Show)
node2 :: (Measured v a) => a -> a -> Node v a
node2 a b = Node2 (measure a <> measure b) a b
      
node3 :: (Measured v a) => a -> a -> a -> Node v a
node3 a b c = Node3 (measure a <> measure b <> measure c) a b c
      
instance (Monoid v) => Measured v (Node v a) where
  measure (Node2 v _ _) = v
  measure (Node3 v _ _ _) = v
  
instance (Monoid v) => Measured v (FingerTree v a) where
  measure Empty = mempty 
  measure (Single v _) = v
  measure (Deep v _ _ _) = v
  
instance (Measured v a) => Measured v [a] where
  measure as = foldr ((\a v -> v <> measure a)) mempty as
  
type Digit a = [a]

data FingerTree v a = Empty
                    | Single v a
                    | Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)
                    deriving (Show)

class Reduce f where
  reducer :: (a -> b -> b) -> (f a -> b -> b)
  
instance Reduce [] where
  reducer f x z = foldr f z x
  
toList :: (Reduce f) => f a -> [a]
toList s = (reducer (:)) s []
       
instance Reduce (Node v) where
  reducer f (Node2 _ a b) c = f a (f b c)
  reducer f (Node3 _ a b c) d = f a (f b (f c d))
  
-- instance Reduce [] where
--   reducer f as = f as
 
mkEmpty :: FingerTree v a
mkEmpty = Empty
        
(<|) :: (Measured v a) => a -> FingerTree v a -> FingerTree v a
a <| Empty = Single (measure a) a
a <| (Single v b) = Deep (measure a <> v) [a] Empty [b]
a <| (Deep v [b, c, d, e] m sf) = Deep (measure a <> v) [a, b] (node3 c d e <| m) sf
a <| (Deep v lf m sf) = Deep (measure a <> v) (a:lf) m sf
  
(|>) :: (Measured v a) => a -> FingerTree v a -> FingerTree v a
a |> Empty = Single (measure a) a
a |> (Single v b) = Deep (measure a <> v) [b] Empty [a]
a |> (Deep v lf m [b, c, d, e]) = Deep (measure a <> v) lf (node3 b c d |> m) [e, a]
a |> (Deep v lf m sf) = Deep (measure a <> v) lf m (sf ++ [a])
  
insertPrio :: (Show a, Measured Max a) => a -> FingerTree Max a -> FingerTree Max a
insertPrio a Empty = a <| Empty
insertPrio a (Single v b) | measure a > v = Deep (measure a <> v) [a] Empty [b]
                          | otherwise = Deep (measure a <> v) [b] Empty [a]
insertPrio a t@(Deep v lf m sf) | measure a > v = a <| t
                                | otherwise = let (l, r) = split2 (measure a) t
                                              in traceShow (l, r) $ a |> l >< r
           
instance Measured Max Float where
  measure f = Max f

t = do
  let t = Empty
  let prios = [0.5, 0.4, 0.3, 0.8, 0.55, 0.55, 0.1, 0.9::Float, 0.55]
  let t' = foldr (insertPrio) t prios
  print t'
           
split :: (Show a, Show v, Measured v a) => (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
split p Empty = (Empty, Empty)
split p xs | p (measure xs) = (l, x <| r)
           | otherwise = traceShow ("otherwise") $ (xs, Empty)
  where Split l x r = splitTree p mempty xs          

split2 :: (Show a, Measured Max a) => Max -> FingerTree Max a -> (FingerTree Max a, FingerTree Max a)
split2 p Empty = (Empty, Empty)
split2 p xs = if p > measure x then (l, x <| r) else (x |> l, r)
  where Split l x r = splitTree2 p xs      

(<||) :: (Measured v a) => [a] -> FingerTree v a -> FingerTree v a
(<||) xs t = foldr (<|) t xs
      
(||>) :: (Measured v a) => [a] -> FingerTree v a -> FingerTree v a
(||>) xs t = foldr (|>) t xs
  
app3 :: (Measured v a) => FingerTree v a -> [a] -> FingerTree v a -> FingerTree v a
app3 Empty ts xs = ts <|| xs
app3 xs ts Empty = ts ||> xs
app3 (Single _ x) ts xs = x <| (ts <|| xs)
app3 xs ts (Single _ x) = x |> (ts ||> xs)
app3 (Deep v1 pr1 m1 sf1) ts (Deep v2 pr2 m2 sf2) = Deep (v1 <> v2 <> measure ts) pr1 (app3 m1 (nodes (sf1 ++ ts ++ pr2)) m2) sf2
     
toTree :: (Measured v a) => [a] -> FingerTree v a
toTree s = s <|| Empty
 
nodes :: (Measured v a) => [a] -> [Node v a]
nodes [a, b] = [node2 a b]
nodes [a, b, c] = [node3 a b c]
nodes [a, b, c, d] = [node2 a b, node2 c d]
nodes (a:b:c:xs) = node3 a b c : nodes xs
      
nodes _ = error "test"
      
(><) :: (Measured v a) => FingerTree v a -> FingerTree v a -> FingerTree v a
xs >< ys = app3 xs [] ys
   
data Split f a = Split (f a) a (f a)

splitDigit :: (Measured v a) => (v -> Bool) -> v -> Digit a -> Split [] a
splitDigit p i [a] = Split [] a []
splitDigit p i (a:as) | p i' = Split [] a as
                      | otherwise = let Split l x r = splitDigit p i' as in Split (a:l) x r
  where i' = i `mappend` measure a                      
  
splitDigit2 :: (Measured Max a) => Max -> Digit a -> Split [] a
splitDigit2 p [a] = Split [] a []
splitDigit2 p (a:as) | p > (measure a) = Split [] a as
                     | otherwise = let Split l x r = splitDigit2 p as in Split (a:l) x r
  
splitTree :: (Show v, Show a, Measured v a) => (v -> Bool) -> v -> FingerTree v a -> Split (FingerTree v) a
splitTree p i (Single _ x) = traceShow ("single", x) $ Split Empty x Empty
splitTree p i (Deep _ pr m sf) | p vpr = let Split l x r = splitDigit p i pr
                                         in traceShow ("p vpr", vpr) $ Split (toTree l) x (deepL r m sf)
                               | p vm = let Split ml xs mr = splitTree p vpr m
                                            Split l x r = splitDigit p (vpr `mappend` measure ml) (toList xs)
                                        in traceShow ("p vm", vm) $ Split (deepR pr m l) x (deepL r mr sf)
                               | otherwise = let Split l x r = splitDigit p vm sf
                                             in traceShow ("otherwise") $ Split (deepR pr m l) x (toTree r)
  where vpr = i `mappend` measure pr
        vm = vpr `mappend` measure m
       
splitTree2 :: (Show a, Measured Max a) => Max -> FingerTree Max a -> Split (FingerTree Max) a
splitTree2 p (Single _ x) = Split Empty x Empty
splitTree2 p (Deep _ pr Empty sf) | p > (measure pr) = let Split l x r = splitDigit2 p pr
                                                       in Split (toTree l) x (toTree $ r ++ sf)
                                  | p > (measure sf) = let Split l x r = splitDigit2 p pr
                                                       in Split (toTree l) x (toTree $ r ++ sf)
                                  | otherwise = let Split l x r = splitDigit2 p sf
                                                in Split (toTree $ pr ++ l) x (toTree r)

splitTree2 p (Deep _ pr m sf) | p > vpr = let Split l x r = splitDigit2 p pr
                                        in traceShow ("p vpr") $ Split (toTree l) x (deepL r m sf)
                              | p > (measure m) = let Split l x r = splitDigit2 p pr
                                                in  traceShow ("p m m") $ Split (toTree l) x (deepL r m sf)
                              | p > (measure sf) = let Split ml xs mr = splitTree2 p m
                                                       Split l x r = splitDigit2 p (toList xs)
                                                   in traceShow ("p m sf") $ Split (deepR pr ml l) x (deepL r mr sf)
                              | otherwise = let Split l x r = splitDigit2 p sf
                                            in traceShow ("otherwise") $ Split (deepR pr m l) x (toTree r)
           
  where vpr = measure pr
  
data View s a = Nil | Cons a (s a)
viewL :: (Measured v a) => FingerTree v a -> View (FingerTree v) a
viewL Empty = Nil
viewL (Single _ x) = Cons x Empty
viewL (Deep _ pr m sf) = Cons (head pr) (deepL (tail pr) m sf)
      
viewR :: (Measured v a) => FingerTree v a -> View (FingerTree v) a
viewR Empty = Nil
viewR (Single _ x) = Cons x Empty
viewR (Deep _ pr m sf) = Cons (last sf) (deepR pr m (init sf))
      
deepL :: (Measured v a) => [a] -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deepL [] m sf = case viewL m of
                  Nil -> toTree sf
                  Cons a m' -> Deep (measure (toList a) <> measure m' <> measure sf) (toList a) m' sf
deepL pr m sf = Deep (measure pr <> measure m <> measure sf) pr m sf
      
deepR :: (Measured v a) => Digit a -> FingerTree v (Node v a) -> [a] -> FingerTree v a
deepR pr m [] = case viewR m of
                  Nil -> toTree pr
                  Cons a m' -> Deep (measure (toList a) <> measure m' <> measure pr) pr m' (toList a)
      
deepR pr m sf = Deep (measure pr <> measure m <> measure sf) pr m sf
      
-- class Monoid v => Measured v a where
--   measure :: a -> v
 
-- newtype MinMax = MinMax (Float, Float) deriving (Show, Eq)
-- instance Monoid MinMax where
--   mempty = MinMax (0, 1)
--   mappend (MinMax (amin, amax)) (MinMax (bmin, bmax)) = MinMax (min amin bmin, max amax bmax)
  
-- instance Ord MinMax where
--   (MinMax (amin, amax)) <= (MinMax (bmin, bmax)) = amax <= bmax

-- data ConceptElem = ConceptElem
--   { _concPriority :: Float
--   } deriving (Show)
  
-- makeLenses ''ConceptElem

-- instance Measured MinMax ConceptElem where
--   measure c = MinMax (c^.concPriority, c^.concPriority)
  
-- instance Measured v a => Measured v (Digit a) where
--   measure (One a) = measure a 
--   measure (Two a b) = measure a `mappend` measure b
--   measure (Three a b c) = measure a `mappend` measure b `mappend` measure c
--   measure (Four a b c d) = measure a `mappend` measure b `mappend` measure c `mappend` measure d
 
-- data Node v a =
--   Node2 v a a
--   | Node3 v a a a
--   deriving (Show)
  
-- instance Measured v a => Measured v (Node v a) where
--   measure (Node2 _ a b) = measure a `mappend` measure b 
--   measure (Node3 _ a b c) = measure a `mappend` measure b `mappend` measure c
  
-- instance Measured v a => Measured v (FingerTree v a) where
--   measure Empty = mempty
--   measure (Single _ a) = measure a
--   measure (Deep _ l m r) = measure l `mappend` measure m `mappend` measure r
  
-- data Digit a =
--   One a
--   | Two a a
--   | Three a a a
--   | Four a a a a
--   deriving (Show)
   
-- data FingerTree v a =  
--   Empty
--   | Single v a
--   | Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)
--   deriving (Show)
  
 
-- digitFromList :: [a] -> Digit a
-- digitFromList [] = error ""
-- digitFromList [a] = One a
-- digitFromList [a, b] = Two a b
-- digitFromList [a, b, c] = Three a b c
-- digitFromList [a, b, c, d] = Four a b c d
-- digitFromList _ = error ""

  
-- addDigitLeft :: a -> Digit a -> Digit a
-- addDigitLeft a (One b) = Two a b
-- addDigitLeft a (Two b c) = Three a b c
-- addDigitLeft a (Three b c d) = Four a b c d
-- addDigitLeft _ (Four _ _ _ _) = error ""

-- addDigitRight :: a -> Digit a -> Digit a
-- addDigitRight a (One b) = Two b a 
-- addDigitRight a (Two b c) = Three b c a
-- addDigitRight a (Three b c d) = Four b c d a
-- addDigitRight _ (Four _ _ _ _) = error ""

-- addMinMax :: (Measured MinMax a) => a -> Digit a -> Digit a
-- addMinMax a (One b) | maxa > maxb = Two a b
--                     | otherwise = Two b a
--   where MinMax (_, maxa) = measure a
--         MinMax (_, maxb) = measure b
-- addMinMax a (Two b c) | maxa > maxb = Three a b c
--                       | maxa > maxc = Three b a c
--                       | otherwise = Three b c a
--   where MinMax (_, maxa) = measure a
--         MinMax (_, maxb) = measure b
--         MinMax (_, maxc) = measure c
-- addMinMax a (Three b c d) | maxa > maxb = Four a b c d
--                           | maxa > maxc = Four b a c d
--                           | maxa > maxd = Four b c a d
--                           | otherwise = Four b c d a
--   where MinMax (_, maxa) = measure a
--         MinMax (_, maxb) = measure b
--         MinMax (_, maxc) = measure c
--         MinMax (_, maxd) = measure d
  
-- addMinMax _ (Four _ _ _ _) = error ""
          
-- splitLeft :: (Measured MinMax a) => a -> Digit a -> (Digit a, Node MinMax a)
-- splitLeft a (Four b c d e) | maxa > maxb = (Two a b, newNode3 c d e)
--                            | maxa > maxc = (Three b a c, newNode2 d e)
--                            | maxa > maxd = (Two b c, newNode3 a d e)
--                            | maxa > maxe = (Two b c, newNode3 d a e)
--                            | otherwise = (Three b d d, newNode2 e a)
--   where MinMax (_, maxa) = measure a
--         MinMax (_, maxb) = measure b
--         MinMax (_, maxc) = measure c
--         MinMax (_, maxd) = measure d
--         MinMax (_, maxe) = measure e

-- splitRight :: (Measured MinMax a) => a -> Digit a -> (Node MinMax a, Digit a)
-- splitRight a (Four b c d e) | maxa > maxb = (newNode2 a b, Three c d e)
--                             | maxa > maxc = (newNode3 b a c, Two d e)
--                             | maxa > maxd = (newNode2 b c, Three a d e)
--                             | maxa > maxe = (newNode2 b c, Three d a e)
--                             | otherwise = (newNode3 b d d, Two e a)
--   where MinMax (_, maxa) = measure a
--         MinMax (_, maxb) = measure b
--         MinMax (_, maxc) = measure c
--         MinMax (_, maxd) = measure d
--         MinMax (_, maxe) = measure e
  
-- splitList :: (Measured MinMax a) => MinMax -> [a] -> ([a], a, [a])
-- splitList _ [] = error ""
-- splitList _ [a] = ([], a, [])
-- splitList p (a:as) | p > measure a = ([], a, as)
--                    | otherwise = let (l, x, r) = splitList p as in (a:l, x, r)
  
-- splitDigit :: (Measured MinMax a) => MinMax -> Digit a -> (Maybe (Digit a), a, Maybe (Digit a))
-- splitDigit x (One b) = (Nothing, b, Nothing)
--   where
--         y = measure b :: MinMax
  
-- splitDigit x (Two b c) | x > y = (Nothing, b, Just (One c))
--                        | otherwise = (Just (One b), c, Nothing)
--   where 
--         y = measure b :: MinMax
--         z = measure c :: MinMax

-- splitDigit x (Three b c d) | x > y = (Nothing, b, Just (Two c d))
--                            | x > z = (Just (One b), c, Just (One d))
--                            | otherwise = (Just (Two b c), d, Nothing)
--   where 
--         y = measure b :: MinMax
--         z = measure c :: MinMax
--         w = measure d :: MinMax
  
-- splitDigit a' (Four b c d e) | a' > b' = (Nothing, b, Just (Three c d e))
--                             | a' > c' = (Just (One b), c, Just (Two d e))
--                             | a' > d' = (Just (Two b c), d, Just (One e))
--                             | otherwise = (Just (Three b c d), e, Nothing)
--   where 
--         b' = measure b :: MinMax
--         c' = measure c :: MinMax
--         d' = measure d :: MinMax
--         e' = measure e :: MinMax
-- -- splitDigit a (Two b c) | (measure a) > (measure b) = (Nothing, a, Just (Two b c))
-- --                        | (measure a) > (measure c) = (Just (One b), a, Just (One c))
-- --                        | otherwise = (Just (Two b c), a, Nothing)
                       
-- -- splitDigit a (Three b c d) | (measure a) > (measure b) = (Nothing, a, Just (One b))
-- --                            | otherwise = undefined
-- -- splitDigit a (Four b c d e) | (measure a) > (measure b) = (Nothing, a, Just (One b))
-- --                             | otherwise = undefined


 
-- newNode2 :: (Measured v a) => a -> a -> Node v a
-- newNode2 a b = Node2 (measure a <> measure b) a b
         
-- newNode3 :: (Measured v a) => a -> a -> a -> Node v a
-- newNode3 a b c = Node3 (measure a <> measure b <> measure c) a b c
-- mkEmpty :: FingerTree v a
-- mkEmpty = Empty
        
-- type Bag a = FingerTree MinMax a
     
-- merge :: FingerTree MinMax a -> FingerTree MinMax a -> FingerTree MinMax a
-- merge = undefined
      
-- class FT v where
--   insert :: (Measured v a) => a -> FingerTree v a -> FingerTree v a
  
-- mDigitToTree :: (FT v, Measured v a) => Maybe (Digit a) -> FingerTree v a
-- mDigitToTree Nothing = mkEmpty
-- mDigitToTree (Just d) = toTree . toList $ d
             
-- mDigitToList :: Maybe (Digit a) -> [a]
-- mDigitToList Nothing = []
-- mDigitToList (Just d) = toList d
             
             
-- _test :: (FT v, Measured v a) => Maybe (Digit a) -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
-- _test Nothing m sf = emptyDeep m (toList sf)
-- _test (Just d) m sf = Main.deep (toList d) m sf
      
-- _test2 :: (FT v, Measured v a) => (Digit a) -> FingerTree v (Node v a) -> Maybe (Digit a) -> FingerTree v a
-- _test2 lf m Nothing = Main.emptyDeep2 (toList lf) m
-- _test2 lf m (Just d) = Main.deep2 lf m (toList d)
     
-- split :: (Measured MinMax a) => MinMax -> FingerTree MinMax a -> (FingerTree MinMax a, a, FingerTree MinMax a)
-- split prio Empty = error ""
-- split prio (Single p a) | prio > p = (Empty, a, Empty)
--                         | otherwise = (Empty, a, Empty)
-- split prio (Deep p lf m sf) | prio > p' = let (ml, a, mr) = splitDigit p lf
--                                           in (mDigitToTree ml, a, _test mr m sf)
--                             | prio < measure sf = let (ml, a, mr) = splitDigit p sf
--                                                   in (_test2 lf m ml, a, mDigitToTree mr)
--                             | otherwise = let (mtl, xs, mtr) = split prio m
--                                               (ml, x, mr) = splitList p (toList xs)
--                                               in (Main.deep (toList lf) mtl (digitFromList ml)
--                                                  , x, Main.deep (mr) mtr sf)
--   where
--     p' = measure m
        
-- instance FT MinMax where
-- --insert :: (Measured MinMax a) => a -> Bag a -> Bag a
--     insert a Empty = Single (measure a) a

--     insert a (Single v b) | maxa > maxt = Deep (v `mappend` measure a) (One a) Empty (One b)
--                           | otherwise = Deep (v `mappend` measure a) (One b) Empty (One a)
--       where MinMax (_, maxa) = measure a
--             MinMax (_, maxt) = v

--     insert a (Deep v lf m sf) | maxa > maxMid = case lf of
--                                                   Four b c d e -> let (d, lt) = splitLeft a lf 
--                                                                   in Deep (v `mappend` measure a) d (insert lt m) sf
--                                                   otherwise -> Deep (v `mappend` measure a) (addMinMax a lf) m sf
--                               | maxR > maxa = case sf of
--                                                 f@(Four b c d e) -> let (lt, d) = splitRight a f 
--                                                                     in Deep (v `mappend` measure a) lf (insert lt m) d
--                                                 _ -> Deep (v `mappend` measure a) lf m (addMinMax a sf)
--                               | otherwise = Deep (v `mappend` measure a) lf (insert (newNode2 a a) m) sf
--       where MinMax (_, maxa) = measure a
--             MinMax (mint, maxt) = v
--             MinMax (minMid, maxMid) = measure m
--             MinMax (minR, maxR) = measure sf

--     insert a (Deep v (Four b c d e) m sf) = Deep (v `mappend` measure a) (Two a b) (insert (newNode3 c d e) m) sf 
--     insert a (Deep v ap m sf) = Deep (v `mappend` measure a) (addDigitLeft a ap) m sf

-- toTree :: (FT v, Measured v a) => [a] -> FingerTree v a
-- toTree = foldr insert mkEmpty
       
-- data View s a = Nil | Cons a (s a)

-- viewLeft :: (FT v, Measured v a) => FingerTree v a -> View (FingerTree v) a
-- viewLeft Empty = Nil
-- viewLeft (Single _ x) = Cons x Empty
-- viewLeft (Deep _ (One a) m sf) = Cons (a) (emptyDeep m (toList sf))
-- viewLeft (Deep _ lf m sf) = Cons (head .toList $ lf) (Main.deep (tail . toList $ lf) m (sf))
         
-- viewRight :: (FT v, Measured v a) => FingerTree v a -> View (FingerTree v) a
-- viewRight Empty = Nil
-- viewRight (Single _ x) = Cons x Empty
-- viewRight (Deep _ lf m (One a)) = Cons a (emptyDeep2 (toList lf) m)
-- viewRight (Deep _ lf m sf) = Cons (last . toList $ sf) (Main.deep2 lf m (init . toList $ sf))
         
-- class Reduce f where
--   reducer :: (a -> b -> b) -> (f a -> b -> b)
  
-- instance Reduce [] where
--   reducer f x z = foldr f z x
  
-- toList :: (Reduce f) => f a -> [a]
-- toList s = (reducer (:)) s []
       
-- instance Reduce (Node v) where
--   reducer f (Node2 _ a b) c = f a (f b c)
--   reducer f (Node3 _ a b c) d = f a (f b (f c d))
  
-- instance Reduce (Digit) where
--   reducer f (One a) b = f a b
--   reducer f (Two a b) c = f a (f b c)
--   reducer f (Three a b c) d = f a (f b (f c d))
--   reducer f (Four a b c d) e = f a (f b (f c (f d e)))
  
-- instance Reduce (FingerTree v) where
--   reducer _ Empty a = a
--   reducer f (Single _ a) b = f a b
--   reducer f (Deep _ pr m sf) z = f' pr (f'' m (f' sf z))
--     where f' = reducer f
--           f'' = reducer . reducer $ f

-- emptyDeep :: (FT v, Measured v a) => FingerTree v (Node v a) -> [a] -> FingerTree v a         
-- emptyDeep m sf' = case viewLeft m of
--                     Nil -> toTree sf'
--                     Cons a m' -> let lf = digitFromList $ toList a 
--                                      sf = digitFromList sf'
--                                  in Deep (measure lf `mappend` measure m' `mappend` measure sf) lf m' sf
          
-- emptyDeep2 :: (FT v, Measured v a) => [a] -> FingerTree v (Node v a) -> FingerTree v a         
-- emptyDeep2 lf' m = case viewRight m of
--                      Nil -> toTree lf'
--                      Cons a m' -> let sf = digitFromList $ toList a
--                                       lf = digitFromList lf'
--                                   in Deep (measure lf `mappend` measure m' `mappend` measure sf) lf m' sf
                              
               

-- deep :: Measured v a => [a] -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
-- deep pr' m sf = Deep (measure pr <> measure m <> measure sf) pr m sf
--   where pr = digitFromList pr'
  
-- deep2 :: Measured v a => Digit a -> FingerTree v (Node v a) -> [a] -> FingerTree v a
-- deep2 pr m sf' = Deep (measure pr <> measure m <> measure sf) pr m sf
--   where sf = digitFromList sf'
       
-- test = do
--   let t = mkEmpty :: FingerTree MinMax ConceptElem
--   let t' = foldr insert t $ map ConceptElem [0.1,0.2..1.0]
--   print t'
  
-- -- append :: a -> FingerTree () a -> FingerTree () a
-- -- append a Empty = Single () a
-- -- append a (Single () b) = Deep () (One a) Empty (One b)
-- -- append a (Deep () (Four b c d e) m sf) = Deep () (Two a b) (Node3 () c d e `append` m) sf
-- -- append a (Deep () (One b) m sf) = Deep () (Two a b) m sf
-- -- append a (Deep () (Two b c) m sf) = Deep () (Three a b c) m sf
-- -- append a (Deep () (Three b c d) m sf) = Deep () (Four a b c d) m s-f

