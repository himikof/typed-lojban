typed-lojban
============

An experimental typing of Lojban grammar, in Haskell.

Trying it out
-------------

Only AST-like datatype construction and convertion to Lojban are supported now.
Convertion to Lojban is implemented as untyping to a `TextTree` (S-expression-like structure), followed by flattening.


Most code resides in `Lojban.Grammar` and `Lojban.Language` modules. 
A few examples:
* `reprTextTree $ untype $ Mi`  
  `"mi"`
* `reprTextTree $ untype $ Bridi1 pelxu defaultSC Ti`  
  `"(ti pelxu)"`
* Specifying context to a selbri in a bridi, swapping sumti places

        reprTextTree $ untype $ Bridi2 zdani defaultSC {hasCu=True,
            places = Just [defaultSP {tag = Just Fe}, defaultSP {tag = Just Fa}]} Ti Zo'e

  `"((fe zo'e) cu zdani (fa ti))"`
* Contexts do not influence (semantic) equality

          Bridi2 zdani defaultSC {hasCu=True, places = Just [defaultSP {tag = Just Fa}]} Ti Zo'e ==
              Bridi2 zdani defaultSC Ti Zo'e
  `True`
* `pelxu` has type `Brivla Nat1` &mdash; it has only one slot

        Bridi2 pelxu defaultSC Zo'e Zo'e`  
  Results in a type error  
* ``reprTextTree $ untype $ tavla `tanruApp` pelxu `tanruApp` zdani``  
  `"((tavla pelxu) zdani)"`
* ``reprTextTree $ untype $ tavla `tanruApp` pelxu `bo` zdani``  
  `"(tavla (pelxu bo zdani))"`
* ``reprTextTree $ untype $ tavla `tanruApp` pelxu `co` zdani``  
  `"((tavla pelxu) co zdani)"`
* ``reprTextTree $ untype $ keKe'e $ zdani `bo` pelxu``  
  `"(ke zdani bo pelxu ke'e)"`
* ``reprTextTree $ untype $ Bridi1 (pelxu `co` zdani) defaultSC Zo'e``  
  `"(zo'e (pelxu co zdani))"`
* ``reprTextTree $ untype $ Bridi1 pelxu defaultSC (lo zdani)``  
   `"((lo zdani) pelxu)"`
* ``reprTextTree $ untype $ loKu zdani``  
  `"(lo zdani ku)"`
* ``reprTextTree $ untype $ lo $ tavla `bo` zdani``  
  `"(lo (tavla bo zdani))"`
* ``reprTextTree $ untype $ withFGTag Ui $ ke $ pelxu `bo` zdani``  
  `"(ke .ui pelxu bo zdani)"`
* ``reprTextTree $ untype $ withFGTagC Ui defaultFGTC {suffixPosition = True} $ keKe'e $ pelxu `bo` zdani``  
   `"((ke pelxu bo zdani ke'e) .ui)"`
* ``reprTextTree $ untype $ withFGTag Ui $ Mi``  
   `"(mi .ui)"`
* ``reprTextTree $ untype $ withFGTag Ui $ lo zdani``  
   `"(lo .ui zdani)"`

Note: The Haskell code here requires GHC 7.6.
