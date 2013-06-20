This module exports high-level lojban grammar structures.

> {-# OPTIONS -XGADTs -XTypeFamilies -XTypeHoles -XMultiParamTypeClasses #-}
> {-# OPTIONS -XFlexibleInstances -XTypeOperators -XFlexibleContexts -XViewPatterns #-}
> {-# OPTIONS -XDeriveGeneric -XStandaloneDeriving -XDeriveDataTypeable -XAutoDeriveTypeable #-}
> {-# OPTIONS -XKindSignatures -XDataKinds -XPolyKinds #-}
> {-# OPTIONS -XOverloadedStrings -XScopedTypeVariables -XUndecidableInstances #-}

> module Lojban.Grammar
> (
>   Word(..),
>   Textful(..),
>   showTextTree, reprTextTree,
>   Sumti(..),
>   Term(..), TermsF(..), TermsState(..), TSDefault, nilTerm,
>   BridiTailF(..), Bridi'(..),
>   bridi1, bridi2, bridi3, bridi4,
>   Selbri(..),
>   Bridi(..), SelbriCtx(..), defaultSC,
>   SelbriPlace(..), defaultSP,
>   FreeGrammarTag(..),
>   FGTaggable(..),
>   FGTransCtx(..), defaultFGTC,
>   --Brivla(..),
>   --Tanru(..),
>   SumtiFGT(..),
>   SelbriFGT(..),
>   --CU(..), KE(..), KEhE(..), TanruApp(..), BO(..), CO(..),
>   --NA(..), KU(..), LE(..),
>   module Lojban.Grammar.Basic,
>   FA(..),
>   tanruApp, bo, co, ke, keKe'e, lo, loKu,
> ) where

> import GHC.Generics hiding (moduleName)
> import Data.Char
> import Data.Functor
> import Data.Typeable
> import Data.List
> import Data.Maybe (catMaybes)
> import Control.Monad.State
> import Lojban.Grammar.TextTree
> import Lojban.Grammar.NatKind
> import Lojban.Grammar.TypeEq
> import Lojban.Grammar.Basic
> import Unsafe.Coerce (unsafeCoerce)

Sumti class:

> --class (Eq w, Textful w, Typeable w, FGTaggable w) => Sumti w where
> data Sumti = Sumti Quantifier SimpleSumti
>   deriving (Eq, Generic, Typeable)
> instance Textful Sumti where
>   untype (Sumti EmptyQ s) = untype s
>   untype (Sumti q s) = mkTNode (liftedUntype (q, s)) [] []

> data SimpleSumti where
>   La :: Word -> SimpleSumti
>   ProSumti :: KOhA -> SimpleSumti
>   LESumti :: forall n. LE -> LESumtiCtx -> Selbri n -> SimpleSumti
> instance Eq SimpleSumti where
>   (La w1) == (La w2) = w1 == w2
>   (ProSumti p1) == (ProSumti p2) = p1 == p2
>   (LESumti le1 ctx1 s1) ==  (LESumti le2 ctx2 s2) = 
>       and [le1 == le2, s1 `eqT1` s2]
> instance Textful SimpleSumti where
>   untype (La w) = TLeaf w
>   untype (ProSumti ps) = untype ps
>   untype (LESumti d c s) = mkTNode [] [untype d] $ liftedUntype (s, mkKu c)

Defining selbri and brivla (arity-constrained):

> data SelbriPlace = SelbriPlace {
>   explicitZo'e :: Bool,
>   tag :: Elidable (FA 0)
> }
> deriving instance Eq (SelbriPlace)
> deriving instance Show (SelbriPlace)
> defaultSP :: SelbriPlace
> defaultSP = SelbriPlace {explicitZo'e = False, tag = Nothing}

> data SelbriCtx = SCtx { 
>   hasCu :: Bool,
>   places :: Maybe [SelbriPlace]
> } deriving (Eq, Show)
> defaultSC :: SelbriCtx
> defaultSC = SCtx { hasCu = False, places = Nothing }

> --class (Eq t, Textful t, Typeable t, FGTaggable t) => Selbri (n :: Nat) t | t -> n where
> --class ({-Eq t, -}Textful t, FGTaggable t) => Selbri t where
> --  type SelbriArity t :: Nat
> data Selbri :: Nat -> * where
>   Brivla :: Word -> Selbri n
>   Tanru :: TanruOp -> TanruOpCtx -> Selbri l -> Selbri r -> Selbri r
>   NASelbri :: Selbri n -> Selbri n

> arityEq :: forall n m. (SingI n, SingI m) => Selbri n -> Selbri m -> Maybe (n :~: m)
> arityEq _ _ = (sing :: Sing n) `singEq` (sing :: Sing m)
> instance EqT Selbri where
>   --b1@(Brivla _) `eqT0` b2@(Brivla _) = b1 `arityEq` b2
>   _ `eqT0` _ = Nothing
> instance Eq (Selbri n) where
>   x@(Brivla w1) == y@(Brivla w2) = and [x `eqT1` y, w1 == w2]
>   _ == _ = False

> instance Textful (Selbri n) where
>   untype (Brivla w) = TLeaf w
>   untype (Tanru op ctx l r) = mkTNode [untype $ mkKe ctx, ul] [untype op]
>                                       [ur, untype $ mkKe'e ctx] where
>       (ul, ur) = untypeArgsOrdered op l r
>       untypeArgsOrdered Co l r = (untype r, untype l)
>       untypeArgsOrdered _ l r = (untype l, untype r)
>   untype (NASelbri s) = mkTNode [untype Na] [untype s] []

> {-data Brivla :: Nat -> * where
>   Brivla :: Word -> Brivla n
> deriving instance Eq (Brivla n)-}

> {-deriving instance Typeable (Brivla)
> instance Textful (Brivla n) where
>   untype (Brivla w) = TLeaf w
> instance FGTaggable (Brivla n) where
>   type FGTagged (Brivla n) = SelbriFGT n
>   withFGTagC = SelbriFGT
> instance Selbri (Brivla n) where
>   type SelbriArity (Brivla n) = n-}

Term definition. Term is a (maybe tagged) sumti or a
termset (compound term) or a KU/NA+KU (indicating no sumti).
The datatype parameter is the place index or a Bool indicating
that the place should receive the next index.
There is no IndexedKuTerm, because that does not make sense
(albeit being grammatically correct).
Note that NaKuTerm is not a selbri per se, but has the same
grammatical structure, meaning 'explicit negation span boundary'.
Therefore, it does not occpuy any selbri slots.
TODO: TaggedTerm, TaggedKuTerm and CompoundTerm constructors.

> data TermPlacement = TPFixed Nat | TPAuto | TPPhantom
> data Term :: TermPlacement -> * where
>   SumtiTerm :: Sumti -> Term TPAuto
>   IndexedTerm :: FA n -> Sumti -> Term (TPFixed n)
>   NaKuTerm :: NA -> KU -> Term TPPhantom
> instance EqT Term where
>   SumtiTerm _ `eqT0` SumtiTerm _ = Just Refl
>   IndexedTerm tag _ `eqT0` IndexedTerm tag' _ = liftEq <$> tag `eqT0` tag'
>   NaKuTerm _ _ `eqT0` NaKuTerm _ _ = Just Refl
>   _ `eqT0` _ = Nothing
> instance Eq (Term n) where
>   SumtiTerm s == SumtiTerm s' = s `eqT` s'
>   IndexedTerm tag s == IndexedTerm tag' s' = and [tag == tag', s `eqT` s']
>   NaKuTerm na ku == NaKuTerm na' ku' = and [na == na', ku == ku']
>   _ == _ = False
> instance Textful (Term n) where
>   untype (SumtiTerm s) = untype s
>   untype (IndexedTerm tag s) = mkTNode [untype tag] [untype s] []
>   untype (NaKuTerm na ku) = mkTNode [] (liftedUntype (na, ku)) []

Term list definition. The data type second parameter encodes
the next place index and place arity (maximum used index).
Essentially, an object of type `TermsF a b` is a
term sequence from state `a :: TermState` to `b :: TermState`.

> data TermsState = TState { nextPlaceIndex :: Nat, placeArity :: Nat }
> type TSDefault = TState 0 0
> newtype instance Sing (a :: TermsState) = STermsState (Integer, Integer)
> instance (SingI a, SingI b) => SingI (TState (a :: Nat) (b :: Nat)) where
>   sing = STermsState (fromSing (sing :: Sing a), fromSing (sing :: Sing b))
> instance SingE (KindOf (TState 0 0)) where
>   type DemoteRep (KindOf (TState 0 0)) = (Integer, Integer)
>   fromSing (STermsState s) = s
> --singTermsStateEq :: Sing (a :: TermsState) -> Sing (b :: TermsState) -> Maybe (a :~: b)
> singEq :: (SingE (kp :: KindIs k), Eq (DemoteRep (kp :: KindIs k))) =>
>   Sing (a :: k) -> Sing (b :: k) -> Maybe (a :~: b)
> singEq s1 s2 | fromSing s1 == fromSing s2 = Just (unsafeCoerce Refl)
>              | otherwise = Nothing
> singTermsStateEq = singEq

> type family TPFixedTermsF (n :: Nat) (s :: TermsState) :: TermsState
> type instance TPFixedTermsF n (TState nextI arity) =
>   TState (n + 1) (Max (n + 1) arity)
> type family TPAutoTermsF (s :: TermsState) :: TermsState
> type instance TPAutoTermsF (TState nextI arity) =
>   TState (nextI + 1) (arity + 1)
> data family TermsF (s0 :: TermsState) :: TermsState -> *
> data instance TermsF s0 sx where
>   TNil :: TermsF s0 s0
>   (:#:) :: (SingI (TPFixedTermsF n s)) =>
>           Term (TPFixed n) -> TermsF s0 s
>               -> TermsF s0 (TPFixedTermsF n s)
>   (:#?) :: (SingI (TPAutoTermsF s)) =>
>           Term TPAuto -> TermsF s0 s
>               -> TermsF s0 (TPAutoTermsF s)
>   (:#|) :: (SingI s) => Term TPPhantom -> TermsF s0 s -> TermsF s0 s
> nilTerm = TNil :: TermsF TSDefault TSDefault
> infixr 5 :#:
> infixr 5 :#?
> infixr 5 :#|

> instance EqT (TermsF s0) where
>   TNil `eqT0` TNil = Just Refl
>   (x :#: xs) `eqT0` (y :#: ys) =
>       let
>           prove :: forall n m x y. (SingI (TPFixedTermsF n x), SingI (TPFixedTermsF m y)) =>
>               n :~: m -> x :~: y -> Maybe (TPFixedTermsF n x :~: TPFixedTermsF m y)
>           prove _ _ = (sing :: Sing (TPFixedTermsF n x)) `singTermsStateEq`
>               (sing :: Sing (TPFixedTermsF m y))
>       in do {p1 <- x `eqT0` y; p2 <- xs `eqT0` ys; prove (lower p1) p2;}
>   (_ :#? xs) `eqT0` (_ :#? ys) = (xs `eqT0` ys) >>= prove where
>       prove :: forall x y. (SingI (TPAutoTermsF x), SingI (TPAutoTermsF y)) => x :~: y
>           -> Maybe (TPAutoTermsF x :~: TPAutoTermsF y)
>       prove _ = (sing :: Sing (TPAutoTermsF x)) `singTermsStateEq`
>           (sing :: Sing (TPAutoTermsF y))
>   (_ :#| xs) `eqT0` (_ :#| ys) = xs `eqT0` ys
>   _ `eqT0` _ = Nothing
>   
> instance Eq (TermsF s0 s) where
>   TNil == TNil = True
>   (x :#: xs) == (y :#: ys) = and [x `eqT1` y, xs `eqT1` ys]
>   (x :#? xs) == (y :#? ys) = and [x == y, xs `eqT1` ys]
>   (x :#| xs) == (y :#| ys) = and [x == y, xs `eqT1` ys]

> untypeTermsF' :: TermsF s0 s -> [TextTree]
> untypeTermsF' TNil = []
> untypeTermsF' (x :#: xs) = untype x : untypeTermsF' xs
> untypeTermsF' (x :#? xs) = untype x : untypeTermsF' xs
> untypeTermsF' (x :#| xs) = untype x : untypeTermsF' xs
> instance Textful (TermsF s0 s) where
>   untype xs = mkTNode (untypeTermsF' xs) [] []

Bridi tail (compound bridi) definitions.
A bridi tail is a divergent part of a logically merged bridi group.
For example: `mi klama le zarci .ije mi nelci la djan.` is equivalent to
`mi klama le zarci gi'e nelci la djan.`. The data type parameter encodes
maximum used place index.

> data family BridiTailF (s :: TermsState) :: TermsState -> *
> data instance BridiTailF s0 sx where
>   SelbriBT :: (arity <= n) => Selbri n -> TermsF s0 (TState ni arity)
>       -> BridiTailF s0 (TState ni arity)
> instance Textful (BridiTailF s0 s) where
>   untype (SelbriBT s ts) = mkTNode [] [untype s] [untype ts]

> data Bridi' :: Nat -> * where
>   Bridi' :: TermsF TSDefault s0 -> BridiTailF s0 (TState ni arity) -> Bridi' arity
> instance Textful (Bridi' arity) where
>   untype (Bridi' prefix tail) = mkTNode [untype prefix] [untype tail] []

Helpers for simple bridi:

> bridi1 :: Selbri 1 -> Sumti -> Bridi' 1
> bridi1 s x1 = Bridi' (SumtiTerm x1 :#? TNil) $ SelbriBT s TNil
> bridi2 :: Selbri 2 -> Sumti -> Sumti -> Bridi' 2
> bridi2 s x1 x2 = Bridi' (SumtiTerm x1 :#? TNil) $ SelbriBT s (SumtiTerm x2 :#? TNil)
> bridi3 :: Selbri 3 -> Sumti -> Sumti -> Sumti -> Bridi' 3
> bridi3 s x1 x2 x3 = Bridi' (SumtiTerm x1 :#? TNil) $ SelbriBT s
>   (SumtiTerm x2 :#? SumtiTerm x3 :#? TNil)
> bridi4 :: Selbri 4 -> Sumti -> Sumti -> Sumti -> Sumti -> Bridi' 4
> bridi4 s x1 x2 x3 x4 = Bridi' (SumtiTerm x1 :#? TNil) $ SelbriBT s
>   (SumtiTerm x2 :#? SumtiTerm x3 :#? SumtiTerm x4 :#? TNil)

FA tag cmavo:

TODO: CLL/9/3: support fi'a (in FA) - place structure question

> data FA :: Nat -> * where
>   Fa :: FA 0
>   Fe :: FA 1
>   Fi :: FA 2
>   Fo :: FA 3
>   Fu :: FA 4
> instance EqT FA where
>   Fa `eqT0` Fa = Just Refl
>   Fe `eqT0` Fe = Just Refl
>   Fi `eqT0` Fi = Just Refl
>   Fo `eqT0` Fo = Just Refl
>   Fu `eqT0` Fu = Just Refl
>   _ `eqT0` _ = Nothing
> deriving instance Eq (FA n)
> deriving instance Show (FA n)

> instance Textful (FA n) where
>   untype Fa = TLeaf "Fa"
>   untype Fe = TLeaf "Fe"
>   untype Fi = TLeaf "Fi"
>   untype Fo = TLeaf "Fo"
>   untype Fu = TLeaf "Fu"
> tagIndex :: forall n. SingI n => FA n -> Int
> tagIndex _ = fromInteger $ fromSing (sing :: Sing n)
> {-tagIndex Fa = 0
> tagIndex Fe = 1
> tagIndex Fi = 2
> tagIndex Fo = 3
> tagIndex Fu = 4-}
> {-mkFA :: Int -> Maybe (FA n)
> mkFA 0 = Just Fa
> mkFA 1 = Just Fe
> mkFA 2 = Just Fi
> mkFA 3 = Just Fo
> mkFA 4 = Just Fu-}

Some helpers for bridi implementation:

> mkCu :: SelbriCtx -> Elidable CU
> mkCu c = if hasCu c then Just Cu else Nothing

> eqT :: (Eq a, Typeable a, Typeable b) => a -> b -> Bool 
> eqT a b = Just a == cast b

Main bridi datatype:

CU is already supported.

Sumti slot remapper (planned):
 - Support moving selbri around (x1 x2 selbri x3, for example).
 - Support zo'e omission
 - Think about exact string preserving after roundtrip.
 - BE/BEI/BEhO linked sumti

TODO: CLL/9/3: Actually, there may be more than one Sumti in a slot, using FA tags.
How can it be typed?

> data Bridi where
>   Bridi1 :: Selbri 1 -> SelbriCtx -> Sumti -> Bridi
>   Bridi2 :: Selbri 2 -> SelbriCtx -> Sumti -> Sumti -> Bridi
>   Bridi3 :: Selbri 3 -> SelbriCtx -> Sumti -> Sumti -> Sumti -> Bridi
>   Bridi4 :: Selbri 4 -> SelbriCtx -> Sumti -> Sumti -> Sumti -> Sumti -> Bridi
> {-instance Eq Bridi where
>   Bridi1 s _ x1 == Bridi1 s' _ x1' = and [s `eqT` s', x1 `eqT` x1']
>   Bridi2 s _ x1 x2 == Bridi2 s' _ x1' x2' = and [s `eqT` s', x1 `eqT` x1', x2 `eqT` x2']
>   Bridi3 s _ x1 x2 x3 == Bridi3 s' _ x1' x2' x3' =
>       and [s `eqT` s', x1 `eqT` x1', x2 `eqT` x2', x3 `eqT` x3']
>   Bridi4 s _ x1 x2 x3 x4 == Bridi4 s' _ x1' x2' x3' x4' =
>       and [s `eqT` s', x1 `eqT` x1', x2 `eqT` x2', x3 `eqT` x3', x4 `eqT` x4']
>   _ == _ = False-}
> instance Textful Bridi where
>   untype (Bridi1 s c x1) = untypeBridi (untype s) c [untype x1]
>   untype (Bridi2 s c x1 x2) = untypeBridi (untype s) c $ liftedUntype (x1, x2)
>   untype (Bridi3 s c x1 x2 x3) = untypeBridi (untype s) c $ liftedUntype (x1, x2, x3)
>   untype (Bridi4 s c x1 x2 x3 x4) = untypeBridi (untype s) c $ liftedUntype (x1, x2, x3, x4)

TODO: Check places structure for correctness.

> untypeBridi :: TextTree -> SelbriCtx -> [TextTree] -> TextTree
> untypeBridi s ctx sumtis = layoutBridi sumtis (untype $ mkCu ctx, s) ctx

> layoutBridiPlaces :: [SelbriPlace] -> [TextTree] -> [Maybe TextTree]
> layoutBridiPlaces places sumtis = evalState (mapM f places) 0 where
>   f :: SelbriPlace -> State Int (Maybe TextTree)
>   f place = 
>       case tag place of
>            Nothing -> do
>               sumti <- liftM (sumtis !!) $ get
>               modify (+1)
>               return $ Just sumti
>            Just tag -> do
>               put $ tagIndex tag + 1
>               let sumti = sumtis !! tagIndex tag
>               return $ Just $ mkTNode [] [untype tag] [sumti]

> layoutBridi :: [TextTree] -> (TextTree, TextTree) -> SelbriCtx -> TextTree
> layoutBridi sumtis (cu, selbri) ctx = mkTNode' $
>   case ctx of
>       (places -> Nothing) -> layout $ catMaybes $ layoutBridiPlaces defPlaces sumtis
>       (places -> Just places) -> layout $ catMaybes $ layoutBridiPlaces places sumtis
>   where defPlace = SelbriPlace {explicitZo'e = False, tag = Nothing}
>         defPlaces = take (length sumtis) $ repeat $ defPlace
>         layout [] = ([cu], [selbri], [])
>         layout (t:ts) = ([t, cu], [selbri], ts)

Tanru - complex selbri, recursively defined.
Operators: 
 - ``Selbri `tanruApp` Selbri``: written as "Selbri Selbri", priority 6, left-associative
 - ``Selbri `bo` Selbri``: priority 9, right-associative
 - ``ke [Selbri]``: written as "ke Selbri Selbri ... Selbri [ke'e]", scoped
 - ``Selbri `JA` Selbri``: logical connectives, priority 8, left-associative
 - ``Selbri `JAbo` Selbri``: logical connectives, priority 7, left-associative
 - ``Selbri `co` Selbri``: inversion, priority 5, right-associative, cannot be inside ke...ke'e

TODO: JA, JAbo, prohibit co inside ke...ke'e

> data KeState = HasKEAndKEhE | HasKE | HasNone

> data TanruOpCtx = TanruOpCtx { keState :: KeState }
> defaultTC :: TanruOpCtx
> defaultTC = TanruOpCtx { keState = HasNone }

> mkKe :: TanruOpCtx -> Elidable KE
> mkKe c = case keState c of
>   HasNone -> Nothing
>   _       -> Just Ke
> mkKe'e :: TanruOpCtx -> Elidable KEhE
> mkKe'e c = case keState c of
>   HasKEAndKEhE -> Just Ke'e
>   _            -> Nothing

> data TanruOp = GroupApp | Bo | Co deriving (Eq, Generic)
> instance Textful TanruOp where
>   untype GroupApp = emptyTNode
>   untype Bo = TLeaf "bo"
>   untype Co = TLeaf "co"

> {-class (Eq w, Textful w, Typeable w) => TanruOp w where
>   untypeArgsOrdered :: w -> Selbri l -> Selbri r -> (TextTree, TextTree)
>   untypeArgsOrdered _ l r = (untype l, untype r)

> data TanruApp = TanruApp deriving (Eq, Typeable)
> instance Textful TanruApp where
>   untype = const emptyTNode
> instance TanruOp TanruApp where

> data BO = Bo deriving (Eq, Generic, Typeable)
> instance Textful BO where
> instance TanruOp BO where

> data CO = Co deriving (Eq, Generic, Typeable)
> instance Textful CO where
> instance TanruOp CO where
>   untypeArgsOrdered _ l r = (untype r, untype l)-}

> {-data Tanru :: Nat -> * where
>   Tanru :: (TanruOp op, Selbri l, Selbri r, SelbriArity r ~ n) =>
>       op -> TanruOpCtx -> l -> r -> Tanru n

> instance (Typeable n) => Selbri (Tanru n) where
>   type SelbriArity (Tanru n) = n
> instance Textful (Tanru n) where
>   untype (Tanru op c l r) = mkTNode [untype $ mkKe c, ul] [untype op]
>                                     [ur, untype $ mkKe'e c] where
>       (ul, ur) = untypeArgsOrdered op l r-}
> {-instance Eq (Tanru n) where 
>   Tanru op _ l r == Tanru op' _ l' r' = and [op `eqT` op', l `eqT` l', r `eqT` r']-}
> {-instance Typeable n => FGTaggable (Tanru n) where
>   type FGTagged (Tanru n) = SelbriFGT n
>   withFGTagC = SelbriFGT-}

> modifyTanruOpCtx :: (TanruOpCtx -> TanruOpCtx) -> Selbri n -> Selbri n
> modifyTanruOpCtx f (Tanru op c l r) = Tanru op (f c) l r

> infixl 6 `tanruApp`
> tanruApp :: Selbri l -> Selbri r -> Selbri r
> l `tanruApp` r = Tanru GroupApp defaultTC l r
> infixr 9 `bo`
> bo :: Selbri l -> Selbri r -> Selbri r
> l `bo` r = Tanru Bo defaultTC l r
> infixr 5 `co`
> co :: Selbri r -> Selbri l -> Selbri r
> r `co` l = Tanru Co defaultTC l r

> updateKeState :: KeState -> Selbri n -> Selbri n
> updateKeState s = modifyTanruOpCtx (\c -> c {keState = s})
> ke :: Selbri n -> Selbri n
> ke = updateKeState HasKE
> keKe'e :: Selbri n -> Selbri n
> keKe'e = updateKeState HasKEAndKEhE

Descriptors - selbri to sumti conversion

TODO: implement other LE members - le, le'e, le'i, lo'e, lo'i, loi, etc

> data LESumtiCtx = LESumtiCtx { hasKu :: Bool }
> defaultLEC :: LESumtiCtx
> defaultLEC = LESumtiCtx { hasKu = False }
> mkKu :: LESumtiCtx -> Elidable KU
> mkKu c = if hasKu c then Just Ku else Nothing

> {-data LESumti where
>   LESumti :: (Selbri s) => LE -> LESumtiCtx -> s -> LESumti-}
> {-instance Eq LESumti where
>   LESumti d _ s == LESumti d' _ s' = and [d == d', s `eqT` s']-}
> {-instance Textful LESumti where
>   untype (LESumti d c s) = mkTNode [] [untype d] $ liftedUntype (s, mkKu c)-}
> {-instance FGTaggable LESumti where
>   type FGTagged LESumti = SumtiFGT
>   withFGTagC = SumtiFGT-}
> --instance Sumti LESumti where

> lo :: Selbri n -> Sumti
> lo = Sumti Su'o . LESumti Lo defaultLEC
> loKu :: Selbri n -> Sumti
> loKu = Sumti Su'o . LESumti Lo defaultLEC { hasKu = True }

Free grammar transformers: attitudinals and such

TODO: implement bridi tagging

> class (Eq t, Typeable t, Textful t) => FreeGrammarTag t where

> class FGTaggable w where
>   type FGTagged w :: *
>   withFGTagC :: (FreeGrammarTag t) => t -> FGTransCtx -> w -> FGTagged w
>   withFGTag :: (FreeGrammarTag t) => t -> w -> FGTagged w
>   withFGTag t = withFGTagC t defaultFGTC

> data FGTransCtx = FGTransCtx { suffixPosition :: Bool }
> defaultFGTC :: FGTransCtx
> defaultFGTC = FGTransCtx { suffixPosition = False }

> defaultFreeGrammarUntype :: (Textful w, FreeGrammarTag t) => 
>                             t -> FGTransCtx -> w -> TextTree
> defaultFreeGrammarUntype t c w = mkTNode' $ if suffixPosition c
>                                             then ([untype w], [untype t], [])
>                                             else case untype w of
>                                                       su@(TLeaf _) -> ([su], [untype t], [])
>                                                       TNode l c r -> (l, c ++ [untype t], r)

> data SumtiFGT where
>   SumtiFGT :: (FreeGrammarTag t) => t -> FGTransCtx -> Sumti -> SumtiFGT
> instance Eq SumtiFGT where
>   SumtiFGT t _ s == SumtiFGT t' _ s' = and [t `eqT` t', s `eqT` s']
> instance Textful SumtiFGT where
>   untype (SumtiFGT t c s) = defaultFreeGrammarUntype t c s
> {-instance FGTaggable SumtiFGT where
>   type FGTagged SumtiFGT = SumtiFGT
>   withFGTagC = SumtiFGT-}
> --instance Sumti SumtiFGT where

> data SelbriFGT :: Nat -> * where
>   SelbriFGT :: (FreeGrammarTag t) =>
>       t -> FGTransCtx -> Selbri n -> SelbriFGT n
> {-instance Eq (SelbriFGT n) where
>   SelbriFGT t _ s == SelbriFGT t' _ s' = and [t `eqT` t', s `eqT` s']-}
> instance Textful (SelbriFGT n) where
>   untype (SelbriFGT t c s) = defaultFreeGrammarUntype t c s
> {-instance FGTaggable (SelbriFGT n) where
>   type FGTagged (SelbriFGT n) = SelbriFGT n
>   withFGTagC = SelbriFGT-}
> {-instance Selbri (SelbriFGT n) where
>   type SelbriArity (SelbriFGT n) = n-}

