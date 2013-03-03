This is a quick testbed for prototyping lojban-related Haskell code.

> {-# OPTIONS -XOverloadedStrings -XDeriveGeneric -XGADTs -XDefaultSignatures #-}
> {-# OPTIONS -XFlexibleInstances -XTypeOperators -XFlexibleContexts #-}
> {-# OPTIONS -XNoMonomorphismRestriction -XKindSignatures -XDataKinds #-}
> {-# OPTIONS -XMultiParamTypeClasses -XStandaloneDeriving -XDeriveDataTypeable #-}
> {-# OPTIONS -XFunctionalDependencies -XTypeFamilies #-}
> import Prelude (Show(..), Eq(..), (.), ($), String, map, undefined, const,
>                 (++), Bool(..), Maybe(..), maybe, error, otherwise, not)
> import GHC.Exts (IsString(..))
> import GHC.Generics hiding (moduleName)
> import Data.Char
> import Data.Foldable
> import Data.Functor
> import Data.Typeable
> import Data.List (intercalate, null, filter)
> import qualified Lojban.Grammar.Typeable as T'

> packageName :: String
> packageName = "lojban"

> moduleName :: String
> moduleName = "Lojban.Grammar"

Some basic definitions:

> newtype Word = Word { unWord :: String } deriving (Show, Eq)
> instance IsString Word where
>   fromString = fixedWord

TextTree type is something similar to S-expressions. It can represent
hierarchical structure of text, but is untyped.

> data TreeT w = TLeaf w | TNode [TreeT w]
> type TextTree = TreeT Word
> instance Foldable TreeT where
>   foldMap f (TLeaf w) = f w
>   foldMap f (TNode l) = foldMap (foldMap f) l
> instance Show (TextTree) where
>   show = showTextTree
> isEmptyTT :: TreeT w -> Bool
> isEmptyTT (TLeaf _) = False
> isEmptyTT (TNode l) = null l
> mkTNode :: [TreeT w] -> TreeT w
> mkTNode l = TNode $ filter (not . isEmptyTT) l

> flatten :: TextTree -> [Word]
> flatten = foldMap (\x -> [x])

> vowels :: String
> vowels = "aeiou"
> isVowel :: Char -> Bool
> isVowel c = (c `elem` vowels)

> fixedWord :: String -> Word
> fixedWord "" = ""
> fixedWord (x:xs) = Word $ prefix ++ xl : xs where
>   xl = toLower x
>   prefix | isVowel xl = "."
>          | otherwise = ""

Textful class contains all typed representations of TextTrees.

> class Eq t => Textful t where
>   untype :: t -> TextTree
>   default untype :: (Generic t, GPhraseUntype (Rep t)) => t -> TextTree
>   untype = guntype . from

Generic phrase untyping:

> class GPhraseUntype f where
>   guntype :: f a -> TextTree
>   guntype = mkTNode . guntype2
>   guntype2 :: f a -> [TextTree]
>   guntype2 x = case guntype x of
>       u@(TLeaf _) -> [u]
>       TNode l -> l
> constructorAsPhrase :: Constructor c => (C1 c t) a -> TextTree
> constructorAsPhrase = TLeaf . fixedWord . conName
> instance (Constructor c) => GPhraseUntype (C1 c U1)  where
>   guntype = constructorAsPhrase
>   --guntype2 = undefined
> instance (Constructor c, GPhraseUntype t) =>
>       GPhraseUntype (C1 c (S1 NoSelector t)) where
>   guntype x = mkTNode $ [constructorAsPhrase x] ++
>       guntype2 (unM1 $ unM1 x)
> --  guntype2 = undefined
> instance GPhraseUntype (Rec0 Word) where
>   guntype (K1 w) = TLeaf w
>   guntype2 (K1 w) = [TLeaf w]
> instance (Generic p, GPhraseUntype (Rep p)) => GPhraseUntype (Par0 p) where
>   guntype (K1 p) = guntype $ from p
>   guntype2 (K1 p) = guntype2 $ from p
> {-instance (GPhraseUntype a, GPhraseUntype b) => GPhraseUntype (a :*: b) where
>   guntype (L1 x) = guntype x -}
> instance (GPhraseUntype a, GPhraseUntype b) => GPhraseUntype (a :+: b) where
>   guntype2 (L1 x) = guntype2 x
>   guntype2 (R1 x) = guntype2 x
>   guntype (L1 x) = guntype x
>   guntype (R1 x) = guntype x
> instance (Datatype d, GPhraseUntype f) => GPhraseUntype (D1 d f) where
>   guntype2 (M1 x) = guntype2 x
>   guntype (M1 x) = guntype x

> showTextTree :: TextTree -> String
> showTextTree = (intercalate " ") . (map unWord) . flatten
> reprTextTree :: TextTree -> String
> reprTextTree (TLeaf w) = unWord w
> reprTextTree (TNode l) = "(" ++ intercalate " " (map reprTextTree l) ++ ")"

Sumti class:

> class (Textful w, Typeable w, FGTaggable w) => Sumti w where

Some pro-sumti (pronouns and such):

> data ProSumti = Mi | Do | Ti | Ta | Tu | Zo'e
>   deriving (Eq, Generic, Typeable)
> instance Textful ProSumti where
> instance FGTaggable ProSumti where
>   type FGTagged ProSumti = SumtiFGT
>   withFGTagC = SumtiFGT
> instance Sumti ProSumti where

Defining cmene (names):

> data Cmene = La Word deriving (Eq, Generic, Typeable)
> instance Textful Cmene where
> instance FGTaggable Cmene where
>   type FGTagged Cmene = SumtiFGT
>   withFGTagC = SumtiFGT
> instance Sumti Cmene where

Kind [of] magic:

> data Nat = Z | Su Nat
> type Nat1 = Su Z
> type Nat2 = Su Nat1
> type Nat3 = Su Nat2
> type Nat4 = Su Nat3
> type Nat5 = Su Nat4

> data HNat :: Nat -> * where
>   HZ :: HNat Z
>   HS :: HNat n -> HNat (Su n)
> deriving instance Eq (HNat (n :: Nat))
> zeroN = HZ
> oneN = HS zeroN
> twoN = HS oneN
> threeN = HS twoN
> fourN = HS threeN
> fiveN = HS fourN

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

> data SelbriCtx = SCtx { hasCu :: Bool } deriving (Eq)
> defaultSC :: SelbriCtx
> defaultSC = SCtx { hasCu = False }

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

> --- x1 (talker) talks to x2 (audience) about x3 (topic) in language x4
> tavla :: Brivla Nat4
> tavla = Brivla fourN "tavla"

> --- x1 is yellow
> pelxu :: Brivla Nat1
> pelxu = Brivla oneN "pelxu"

> --- x1 gives x2 to x3 (without payment)
> dunda :: Brivla Nat3
> dunda = Brivla threeN "dunda"

> --- x1 is a nest/house/lair/den/[home] of/for x2
> zdani :: Brivla Nat2
> zdani = Brivla twoN "zdani"

CU cmavo:

> data CU = Cu deriving (Eq, Generic)
> instance Textful CU where

Elidable functor:

> type Elidable = Maybe
> instance Textful t => Textful (Elidable t) where
>   untype = maybe (TNode []) untype

Some helpers for bridi implementation:

> mkCu :: SelbriCtx -> Elidable CU
> mkCu c = if hasCu c then Just Cu else Nothing
>
> 
>
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
 - Support FA tags and moving selbri around.
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
>   untype (Bridi1 s c x1) = mkTNode $ liftedUntype (x1, mkCu c, s)
>   untype (Bridi2 s c x1 x2) = mkTNode $ liftedUntype (x1, mkCu c, s, x2)
>   untype (Bridi3 s c x1 x2 x3) = mkTNode $ liftedUntype (x1, mkCu c, s, x2, x3)
>   untype (Bridi4 s c x1 x2 x3 x4) = mkTNode $ liftedUntype (x1, mkCu c, s, x2, x3, x4)

Tanru - complex selbri, recursively defined.
Operators: 
 - Selbri `tanruApp` Selbri : written as "Selbri Selbri", priority 3, left-associative
 - Selbri `bo` Selbri : priority 6, right-associative
 - ke [Selbri] : written as "ke Selbri Selbri ... Selbri [ke'e]", priority 7
 - Selbri `JA` Selbri : logical connectives, priority 5, left-associative
 - Selbri `JAbo` Selbri : logical connectives, priority 4, left-associative
 - Selbri `co` Selbri : inversion, priority 2, right-associative, cannot be inside ke...ke'e

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
>   untype = const $ TNode []
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
>   untype (Tanru op c l r) = mkTNode $ [untype $ mkKe c, ul, untype op,
>                                         ur, untype $ mkKe'e c] where
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

> tanruApp :: (Selbri m l, Selbri n r) => l -> r -> Tanru n
> l `tanruApp` r = Tanru TanruApp defaultTC l r
> bo :: (Selbri m l, Selbri n r) => l -> r -> Tanru n
> l `bo` r = Tanru Bo defaultTC l r
> co :: (Selbri m l, Selbri n r) => l -> r -> Tanru n
> l `co` r = Tanru Co defaultTC l r

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
>   untype (LESumti d c s) = mkTNode $ liftedUntype (d, s, mkKu c)
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
>                             String -> t -> FGTransCtx -> w -> TextTree
> defaultFreeGrammarUntype msg t c w = mkTNode $ if suffixPosition c
>                                                then liftedUntype (w, t)
>                                                else case untype w of
>                                                   su@(TLeaf _) -> [su, untype t]
>                                                   TNode [] -> error msg
>                                                   TNode (x:xs) -> x:untype t:xs

> data SumtiFGT where
>   SumtiFGT :: (Sumti s, FreeGrammarTag t) => t -> FGTransCtx -> s -> SumtiFGT
> deriving instance (Typeable SumtiFGT)
> instance Eq SumtiFGT where
>   SumtiFGT t _ s == SumtiFGT t' _ s' = and [t `eqT` t', s `eqT` s']
> instance Textful SumtiFGT where
>   untype (SumtiFGT t c s) = defaultFreeGrammarUntype "Empty sumti" t c s
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
>   untype (SelbriFGT t c s) = defaultFreeGrammarUntype "Empty selbri" t c s
> instance T'.Typeable n => FGTaggable (SelbriFGT n) where
>   type FGTagged (SelbriFGT n) = SelbriFGT n
>   withFGTagC = SelbriFGT
> instance T'.Typeable n => Selbri n (SelbriFGT n) where

Attitudinals implementation:

> data UI = Ui deriving (Eq, Generic, Typeable)
> instance Textful UI where
> instance FreeGrammarTag UI where

> data CAI = Cai|Cu'i|Pei|Sai|Ne'e deriving (Eq, Generic, Typeable)
> instance Textful CAI where
> instance FreeGrammarTag CAI where

Universal negator NAI:
It can be used as a FreeGrammarTag, among other places.

> data NAI = Nai deriving (Eq, Generic, Typeable)
> instance Textful NAI where
> instance FreeGrammarTag NAI where