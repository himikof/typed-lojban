This module exports high-level lojban grammar structures.

> {-# OPTIONS -XGADTs -XFunctionalDependencies -XTypeFamilies -XMultiParamTypeClasses #-}
> {-# OPTIONS -XFlexibleInstances -XTypeOperators -XFlexibleContexts -XViewPatterns #-}
> {-# OPTIONS -XDeriveGeneric -XStandaloneDeriving -XDeriveDataTypeable #-}
> {-# OPTIONS -XKindSignatures -XDataKinds #-}

> module Lojban.Grammar
> (
>   Word(..),
>   Textful(..),
>   showTextTree, reprTextTree,
>   Sumti(..),
>   Selbri(..),
>   Bridi(..), SelbriCtx(..), defaultSC,
>   SelbriPlace(..), defaultSP,
>   FreeGrammarTag(..),
>   FGTaggable(..),
>   FGTransCtx(..), defaultFGTC,
>   Brivla(..),
>   Tanru(..),
>   SumtiFGT(..),
>   SelbriFGT(..),
>   CU(..), KE(..), KEhE(..), TanruApp(..), BO(..), CO(..),
>   KU(..), LE(..), FA(..),
>   tanruApp, bo, co, ke, keKe'e, lo, loKu,
> ) where

> import GHC.Generics hiding (moduleName)
> import Data.Char
> import Data.Functor
> import Data.Typeable
> import Data.List
> import Data.Maybe (catMaybes)
> import Control.Monad.State
> import qualified Lojban.Grammar.Typeable as T'
> import Lojban.Grammar.TextTree
> import Lojban.Grammar.NatKind

> packageName :: String
> packageName = "lojban"

> moduleName :: String
> moduleName = "Lojban.Grammar"

Sumti class:

> class (Textful w, Typeable w, FGTaggable w) => Sumti w where

T'.Typeable is kind-polymorphic advanced Typeable class.
Define T'.Typeable instances for datatypes with unusual kinds:

> defaultSingTyRep :: String -> TypeRep
> defaultSingTyRep s = mkTyConApp (mkTyCon3 packageName moduleName s) []
> instance T'.Typeable 'Z where
>   typeOf = \_ -> defaultSingTyRep "'Z"
> instance T'.Typeable 'Su where
>   typeOf = \_ -> defaultSingTyRep "'Su"
> instance T'.Typeable HNat where
>   typeOf = \_ -> defaultSingTyRep "HNat"

Defining selbri and brivla (arity-constrained):

> data SelbriPlace = SelbriPlace {
>   explicitZo'e :: Bool,
>   tag :: Elidable FA
> } deriving (Eq, Show)
> defaultSP :: SelbriPlace
> defaultSP = SelbriPlace {explicitZo'e = False, tag = Nothing}

> data SelbriCtx = SCtx { 
>   hasCu :: Bool,
>   places :: Maybe [SelbriPlace]
> } deriving (Eq, Show)
> defaultSC :: SelbriCtx
> defaultSC = SCtx { hasCu = False, places = Nothing }

> class (Textful t, Typeable t, FGTaggable t) => Selbri (n :: Nat) t | t -> n where

> data Brivla :: Nat -> * where
>   Brivla :: HNat n -> Word -> Brivla n
> deriving instance Eq (Brivla n)

Brivla cannot be in Typeable1, because its kind is Nat -> *, not * -> *.
So we make use of T'.Typeable and instance (T'.Typeable t, T'.Typeable a) => Typeable (t a).
So Brivla Nat1, for example, is in Typeable as well as T'.Typeable.

> instance T'.Typeable (Brivla) where
>   typeOf = \_ -> rep where
>       rep = defaultSingTyRep "Brivla"
> instance Textful (Brivla n) where
>   untype (Brivla _ w) = TLeaf w
> instance T'.Typeable n => FGTaggable (Brivla n) where
>   type FGTagged (Brivla n) = SelbriFGT n
>   withFGTagC = SelbriFGT
> instance T'.Typeable n => Selbri n (Brivla n) where

FA tag cmavo:

TODO: CLL/9/3: support fi'a (in FA) - place structure question

> data FA = Fa | Fe | Fi | Fo | Fu deriving (Eq, Generic, Show)
> instance Textful FA where
> tagIndex :: FA -> Int
> tagIndex Fa = 0
> tagIndex Fe = 1
> tagIndex Fi = 2
> tagIndex Fo = 3
> tagIndex Fu = 4
> mkFA :: Int -> Maybe FA
> mkFA 0 = Just Fa
> mkFA 1 = Just Fe
> mkFA 2 = Just Fi
> mkFA 3 = Just Fo
> mkFA 4 = Just Fu

CU cmavo:

> data CU = Cu deriving (Eq, Generic)
> instance Textful CU where

Elidable functor:

> type Elidable = Maybe
> instance Textful t => Textful (Elidable t) where
>   untype = maybe emptyTNode untype

Some helpers for bridi implementation:

> mkCu :: SelbriCtx -> Elidable CU
> mkCu c = if hasCu c then Just Cu else Nothing

> class GLiftedUntype f where
>   gliftedUntype :: f a -> [TextTree]
> instance GLiftedUntype U1 where
>   gliftedUntype U1 = []
> instance (GLiftedUntype a, GLiftedUntype b) => GLiftedUntype (a :*: b) where
>   gliftedUntype (a :*: b) = gliftedUntype a ++ gliftedUntype b
> instance (GLiftedUntype a) => GLiftedUntype (M1 i c a) where
>   gliftedUntype (M1 x) = gliftedUntype x
> instance (Textful a) => GLiftedUntype (K1 i a) where
>   gliftedUntype (K1 x) = [untype x]

> liftedUntype :: (Generic a, GLiftedUntype (Rep a)) => a -> [TextTree]
> liftedUntype = gliftedUntype . from

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
>   Bridi1 :: (Selbri Nat1 s, Sumti x1)
>       => s -> SelbriCtx -> x1 -> Bridi
>   Bridi2 :: (Selbri Nat2 s, Sumti x1, Sumti x2)
>       => s -> SelbriCtx -> x1 -> x2 -> Bridi
>   Bridi3 :: (Selbri Nat3 s, Sumti x1, Sumti x2, Sumti x3)
>       => s -> SelbriCtx -> x1 -> x2 -> x3 -> Bridi
>   Bridi4 :: (Selbri Nat4 s, Sumti x1, Sumti x2, Sumti x3, Sumti x4)
>       => s -> SelbriCtx -> x1 -> x2 -> x3 -> x4 -> Bridi
> instance Eq Bridi where
>   Bridi1 s _ x1 == Bridi1 s' _ x1' = and [s `eqT` s', x1 `eqT` x1']
>   Bridi2 s _ x1 x2 == Bridi2 s' _ x1' x2' = and [s `eqT` s', x1 `eqT` x1', x2 `eqT` x2']
>   Bridi3 s _ x1 x2 x3 == Bridi3 s' _ x1' x2' x3' =
>       and [s `eqT` s', x1 `eqT` x1', x2 `eqT` x2', x3 `eqT` x3']
>   Bridi4 s _ x1 x2 x3 x4 == Bridi4 s' _ x1' x2' x3' x4' =
>       and [s `eqT` s', x1 `eqT` x1', x2 `eqT` x2', x3 `eqT` x3', x4 `eqT` x4']
>   _ == _ = False
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

KE and KEhE cmavo:

> data KE = Ke deriving (Eq, Generic)
> instance Textful KE where
> data KEhE = Ke'e deriving (Eq, Generic)
> instance Textful KEhE where

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

> class (Textful w, Typeable w) => TanruOp w where
>   untypeArgsOrdered :: (Selbri m l, Selbri n r) => w -> l -> r -> (TextTree, TextTree)
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
>   untypeArgsOrdered _ l r = (untype r, untype l)

> data Tanru :: Nat -> * where
>   Tanru :: (TanruOp op, Selbri m l, Selbri n r) => op -> TanruOpCtx -> l -> r -> Tanru n

> instance (T'.Typeable n) => Selbri n (Tanru n) where
> instance Textful (Tanru n) where
>   untype (Tanru op c l r) = mkTNode [untype $ mkKe c, ul] [untype op]
>                                     [ur, untype $ mkKe'e c] where
>       (ul, ur) = untypeArgsOrdered op l r
> instance Eq (Tanru n) where 
>   Tanru op _ l r == Tanru op' _ l' r' = and [op `eqT` op', l `eqT` l', r `eqT` r']
> instance T'.Typeable Tanru where
>   typeOf = \_ -> rep where
>       rep = defaultSingTyRep "Tanru"
> instance T'.Typeable n => FGTaggable (Tanru n) where
>   type FGTagged (Tanru n) = SelbriFGT n
>   withFGTagC = SelbriFGT

> modifyTanruOpCtx :: (TanruOpCtx -> TanruOpCtx) -> Tanru n -> Tanru n
> modifyTanruOpCtx f (Tanru op c l r) = Tanru op (f c) l r

> infixl 6 `tanruApp`
> tanruApp :: (Selbri m l, Selbri n r) => l -> r -> Tanru n
> l `tanruApp` r = Tanru TanruApp defaultTC l r
> infixr 9 `bo`
> bo :: (Selbri m l, Selbri n r) => l -> r -> Tanru n
> l `bo` r = Tanru Bo defaultTC l r
> infixr 5 `co`
> co :: (Selbri m l, Selbri n r) => r -> l -> Tanru n
> r `co` l = Tanru Co defaultTC l r

> updateKeState :: KeState -> Tanru n -> Tanru n
> updateKeState s = modifyTanruOpCtx (\c -> c {keState = s})
> ke :: Tanru n -> Tanru n
> ke = updateKeState HasKE
> keKe'e :: Tanru n -> Tanru n
> keKe'e = updateKeState HasKEAndKEhE

KU cmavo:

> data KU = Ku deriving (Eq, Generic)
> instance Textful KU where

Descriptors - selbri to sumti conversion

TODO: implement other LE members - le, le'e, le'i, lo'e, lo'i, loi, etc

> data LESumtiCtx = LESumtiCtx { hasKu :: Bool }
> defaultLEC :: LESumtiCtx
> defaultLEC = LESumtiCtx { hasKu = False }
> mkKu :: LESumtiCtx -> Elidable KU
> mkKu c = if hasKu c then Just Ku else Nothing

> data LE = Lo deriving (Eq, Generic, Typeable)
> instance Textful LE where
> lo :: (Selbri n s) => s -> LESumti
> lo = LESumti Lo defaultLEC
> loKu :: (Selbri n s) => s -> LESumti
> loKu = LESumti Lo defaultLEC { hasKu = True }

> data LESumti where
>   LESumti :: (Selbri n s) => LE -> LESumtiCtx -> s -> LESumti
> deriving instance (Typeable LESumti)
> instance Eq LESumti where
>   LESumti d _ s == LESumti d' _ s' = and [d == d', s `eqT` s']
> instance Textful LESumti where
>   untype (LESumti d c s) = mkTNode [] [untype d] $ liftedUntype (s, mkKu c)
> instance FGTaggable LESumti where
>   type FGTagged LESumti = SumtiFGT
>   withFGTagC = SumtiFGT
> instance Sumti LESumti where

Free grammar transformers: attitudinals and such

TODO: implement bridi tagging

> class (Typeable t, Textful t) => FreeGrammarTag t where

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
>   SumtiFGT :: (Sumti s, FreeGrammarTag t) => t -> FGTransCtx -> s -> SumtiFGT
> deriving instance (Typeable SumtiFGT)
> instance Eq SumtiFGT where
>   SumtiFGT t _ s == SumtiFGT t' _ s' = and [t `eqT` t', s `eqT` s']
> instance Textful SumtiFGT where
>   untype (SumtiFGT t c s) = defaultFreeGrammarUntype t c s
> instance FGTaggable SumtiFGT where
>   type FGTagged SumtiFGT = SumtiFGT
>   withFGTagC = SumtiFGT
> instance Sumti SumtiFGT where

> data SelbriFGT :: Nat -> * where
>   SelbriFGT :: (Selbri n s, FreeGrammarTag t) => t -> FGTransCtx -> s -> SelbriFGT n
> instance T'.Typeable SelbriFGT where
>   typeOf = \_ -> rep where
>       rep = defaultSingTyRep "SelbriFGT"
> instance Eq (SelbriFGT n) where
>   SelbriFGT t _ s == SelbriFGT t' _ s' = and [t `eqT` t', s `eqT` s']
> instance Textful (SelbriFGT n) where
>   untype (SelbriFGT t c s) = defaultFreeGrammarUntype t c s
> instance T'.Typeable n => FGTaggable (SelbriFGT n) where
>   type FGTagged (SelbriFGT n) = SelbriFGT n
>   withFGTagC = SelbriFGT
> instance T'.Typeable n => Selbri n (SelbriFGT n) where

