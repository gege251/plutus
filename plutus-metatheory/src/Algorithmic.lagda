\begin{code}
module Algorithmic where
\end{code}

## Imports

\begin{code}
open import Function hiding (_∋_)
open import Data.Product renaming (_,_ to _,,_)
open import Data.List hiding ([_])
open import Relation.Binary.PropositionalEquality hiding ([_])
open import Data.Unit
open import Data.Sum

open import Utils hiding (TermCon)
open import Type
open import Type.BetaNormal
import Type.RenamingSubstitution as ⋆
open import Type.BetaNBE
open import Type.BetaNBE.RenamingSubstitution renaming (_[_]Nf to _[_])
open import Builtin
open import Builtin.Constant.Term Ctx⋆ Kind * _⊢Nf⋆_ con
open import Builtin.Constant.Type Ctx⋆ (_⊢Nf⋆ *)
\end{code}

## Fixity declarations

To begin, we get all our infix declarations out of the way.
We list separately operators for judgements, types, and terms.
\begin{code}
infix  4 _∋_
infix  4 _⊢_
infixl 5 _,_
\end{code}

## Contexts and erasure

We need to mutually define contexts and their
erasure to type contexts.
\begin{code}
--data Ctx : Set
--∥_∥ : Ctx → Ctx⋆
\end{code}

A context is either empty, or extends a context by
a type variable of a given kind, or extends a context
by a variable of a given type.
\begin{code}
data Ctx : Ctx⋆ → Set where
  ∅    : Ctx ∅
  _,⋆_ : ∀{Φ} → Ctx Φ → (J : Kind) → Ctx (Φ ,⋆ J)
  _,_  : ∀ {Φ} (Γ : Ctx Φ) → Φ ⊢Nf⋆ * → Ctx Φ
\end{code}
Let `Γ` range over contexts.  In the last rule,
the type is indexed by the erasure of the previous
context to a type context and a kind.

The erasure of a context is a type context.
\begin{code}
--∥ ∅ ∥       =  ∅
--∥ Γ ,⋆ J ∥  =  ∥ Γ ∥ ,⋆ J
--∥ Γ , A ∥   =  ∥ Γ ∥
\end{code}

## Variables

A variable is indexed by its context and type.
\begin{code}
open import Type.BetaNormal.Equality
data _∋_ : ∀ {Φ} (Γ : Ctx Φ) → Φ ⊢Nf⋆ * → Set where

  Z : ∀ {Φ Γ} {A : Φ ⊢Nf⋆ *}
      ----------
    → Γ , A ∋ A

  S : ∀ {Φ Γ} {A : Φ ⊢Nf⋆ *} {B : Φ ⊢Nf⋆ *}
    → Γ ∋ A
      ----------
    → Γ , B ∋ A

  T : ∀ {Φ Γ K}{A : Φ ⊢Nf⋆ *}
    → Γ ∋ A
      -------------------
    → Γ ,⋆ K ∋ weakenNf A
\end{code}
Let `x`, `y` range over variables.

## Terms

A term is indexed over by its context and type.  A term is a variable,
an abstraction, an application, a type abstraction, or a type
application.
\begin{code}
sig : Builtin → Σ Ctx⋆ λ Φ → Ctx Φ × Φ ⊢Nf⋆ *
sig ifThenElse = _ ,, ∅ ,⋆ * , con bool , ne (` Z) , ne (` Z) ,, ne (` Z)
sig addInteger = _ ,, ∅ , con integer , con integer ,, con integer
sig subtractInteger = _ ,, ∅ , con integer , con integer ,, con integer
sig multiplyInteger = _ ,, ∅ , con integer , con integer ,, con integer
sig divideInteger = _ ,, ∅ , con integer , con integer ,, con integer
sig quotientInteger = _ ,, ∅ , con integer , con integer ,, con integer
sig remainderInteger = _ ,, ∅ , con integer , con integer ,, con integer
sig modInteger = _ ,, ∅ , con integer , con integer ,, con integer
sig lessThanInteger = _ ,, ∅ , con integer , con integer ,, con bool
sig lessThanEqualsInteger = _ ,, ∅ , con integer , con integer ,, con bool
sig equalsInteger = _ ,, ∅ , con integer , con integer ,, con bool
sig appendByteString = _ ,, ∅ , con bytestring , con bytestring ,, con bytestring
sig lessThanByteString = _ ,, ∅ , con bytestring , con bytestring ,, con bool
sig lessThanEqualsByteString = _ ,, ∅ , con bytestring , con bytestring ,, con bool
sig sha2-256 = _ ,, ∅ , con bytestring ,, con bytestring
sig sha3-256 = _ ,, ∅ , con bytestring ,, con bytestring
sig verifyEd25519Signature = _ ,, ∅ , con bytestring , con bytestring , con bytestring ,, con bool
sig equalsByteString = _ ,, ∅ , con bytestring , con bytestring ,, con bool
sig appendString = _ ,, ∅ , con string , con string ,, con string
sig trace = _ ,, ∅ ,⋆ * , con string , ne (` Z) ,, ne (` Z)
sig equalsString = _ ,, ∅ , con string , con string ,, con bool
sig encodeUtf8 = _ ,, ∅ , con string ,, con bytestring
sig decodeUtf8 = _ ,, ∅ , con bytestring ,, con string
sig fstPair =
  _ ,, ∅ ,⋆ * ,⋆ * , con (pair (ne (` (S Z))) (ne (` Z))) ,, ne (` (S Z))
sig sndPair = 
  _ ,, ∅ ,⋆ * ,⋆ * , con (pair (ne (` (S Z))) (ne (` Z))) ,, ne (` Z)
sig nullList = _ ,, ∅ ,⋆ * , con (list (ne (` Z))) ,, con bool
sig headList = _ ,, ∅ ,⋆ * , con (list (ne (` Z))) ,, ne (` Z)
sig tailList = _ ,, ∅ ,⋆ * , con (list (ne (` Z))) ,, con (list (ne (` Z)))
sig chooseList =
  _
  ,,
  ∅ ,⋆ * ,⋆ * , ne (` (S Z)) , ne (` (S Z)) , con (list (ne (` Z)))
  ,,
  ne (` (S Z)) 
sig constrData = _ ,, ∅ , con integer , con (list (con Data)) ,, con Data
sig mapData = _ ,, ∅ , con (pair (con Data) (con Data)) ,, con Data
sig listData = _ ,, ∅ , con (list (con Data)) ,, con Data
sig iData = _ ,, ∅ , con integer ,, con Data
sig bData = _ ,, ∅ , con bytestring ,, con Data
sig unConstrData =
  _ ,, ∅ , con Data ,, con (pair (con integer) (con (list (con Data))))
sig unMapData = _ ,, ∅ , con Data ,, con (pair (con Data) (con Data))
sig unListData = _ ,, ∅ , con Data ,, con (list (con Data))
sig unIData = _ ,, ∅ , con Data ,, con integer
sig unBData = _ ,, ∅ , con Data ,, con bytestring
sig equalsData = _ ,, ∅ , con Data , con Data ,, con bool
sig serialiseData = _ ,, ∅ , con Data ,, con bytestring
sig chooseData =
  _
  ,,
  ∅ ,⋆ * , ne (` Z) , ne (` Z) , ne (` Z) , ne (` Z) , ne (` Z) , con Data
  ,,
  ne (` Z)
sig chooseUnit = _ ,, ∅ ,⋆ * , ne (` Z) , con unit ,, ne (` Z)
sig mkPairData =
  _ ,, ∅ , con Data , con Data ,, con (pair (con Data) (con Data)) 
sig mkNilData = _ ,, ∅ , con unit ,, con (list (con Data))
sig mkNilPairData = _ ,, ∅ , con unit ,, con (list (con (pair (con Data) (con Data))))
sig mkCons =
  _ ,, ∅ , con Data , con (list (con Data)) ,, con (list (con Data))
sig consByteString = _ ,, ∅ , con integer , con bytestring ,, con bytestring
sig sliceByteString =
  _ ,, ∅ , con integer , con integer , con bytestring ,, con bytestring
sig lengthOfByteString = _ ,, ∅ , con bytestring ,, con integer
sig indexByteString = _ ,, ∅ , con bytestring , con integer ,, con integer
sig blake2b-256 = _ ,, ∅ , con bytestring ,, con bytestring

sig2type : (Φ : Ctx⋆) → Ctx Φ → Φ ⊢Nf⋆ * → ∅ ⊢Nf⋆ *
sig2type .∅ ∅ C = C
sig2type (Φ ,⋆ J) (Γ ,⋆ J) C = sig2type Φ Γ (Π C)
sig2type Φ        (Γ ,  A) C = sig2type Φ Γ (A ⇒ C)

btype : ∀{Φ} → Builtin → Φ ⊢Nf⋆ *
btype b = let Φ ,, Γ ,, C = sig b in subNf (λ()) (sig2type Φ Γ C)

postulate btype-ren : ∀{Φ Ψ} b (ρ : ⋆.Ren Φ Ψ) → btype b ≡ renNf ρ (btype b)
postulate btype-sub : ∀{Φ Ψ} b (ρ : SubNf Φ Ψ) → btype b ≡ subNf ρ (btype b)

infixl 7 _·⋆_/_

data _⊢_ {Φ} (Γ : Ctx Φ) : Φ ⊢Nf⋆ * → Set where

  ` : ∀ {A : Φ ⊢Nf⋆ *}
    → Γ ∋ A
      -----
    → Γ ⊢ A

  ƛ : ∀ {A B : Φ ⊢Nf⋆ *}
    → Γ , A ⊢ B
      ---------
    → Γ ⊢ A ⇒ B

  _·_ : ∀ {A B : Φ ⊢Nf⋆ *}
    → Γ ⊢ A ⇒ B
    → Γ ⊢ A
      ---------
    → Γ ⊢ B

  Λ : ∀ {K}
    → {B : Φ ,⋆ K ⊢Nf⋆ *}
    → Γ ,⋆ K ⊢ B
      -------------------
    → Γ ⊢ Π B

  _·⋆_/_ : ∀ {K C}
    → {B : Φ ,⋆ K ⊢Nf⋆ *}
    → Γ ⊢ Π B
    → (A : Φ ⊢Nf⋆ K)
    → C ≡ B [ A ]
      --------------
    → Γ ⊢ C

  wrap : ∀{K}
   → (A : Φ ⊢Nf⋆ (K ⇒ *) ⇒ K ⇒ *)
   → (B : Φ ⊢Nf⋆ K)
   → Γ ⊢ nf (embNf A · ƛ (μ (embNf (weakenNf A)) (` Z)) · embNf B)
     -------------------------------------------------------------
   → Γ ⊢ μ A B

  unwrap : ∀{K C}
    → {A : Φ ⊢Nf⋆ (K ⇒ *) ⇒ K ⇒ *}
    → {B : Φ ⊢Nf⋆ K}
    → Γ ⊢ μ A B
    → C ≡ nf (embNf A · ƛ (μ (embNf (weakenNf A)) (` Z)) · embNf B)
      -------------------------------------------------------------
    → Γ ⊢ C

  con : ∀{tcn}
    → TermCon {Φ} (con tcn)
      ---------------------
    → Γ ⊢ con tcn

  builtin_/_ : ∀{C}
    → (b :  Builtin)
    → C ≡ btype b
      --------------
    → Γ ⊢ C

  error :
      (A : Φ ⊢Nf⋆ *)
      --------------
    → Γ ⊢ A
\end{code}

Utility functions

\begin{code}
open import Type.BetaNormal.Equality

conv∋ : ∀ {Φ Γ Γ'}{A A' : Φ ⊢Nf⋆ *}
 → Γ ≡ Γ'
 → A ≡ A'
 → Γ ∋ A
 → Γ' ∋ A'
conv∋ refl refl x = x

open import Type.BetaNBE.Completeness
open import Type.Equality
open import Type.BetaNBE.RenamingSubstitution

conv⊢ : ∀ {Φ Γ Γ'}{A A' : Φ ⊢Nf⋆ *}
 → Γ ≡ Γ'
 → A ≡ A'
 → Γ ⊢ A
 → Γ' ⊢ A'
conv⊢ refl refl t = t

Ctx2type : ∀{Φ}(Γ : Ctx Φ) → Φ ⊢Nf⋆ * → ∅ ⊢Nf⋆ *
Ctx2type ∅        C = C
Ctx2type (Γ ,⋆ J) C = Ctx2type Γ (Π C)
Ctx2type (Γ , x)  C = Ctx2type Γ (x ⇒ C)

data Arg : Set where
  Term Type : Arg

Arity = List Arg

ctx2bwdarity : ∀{Φ}(Γ : Ctx Φ) → Bwd Arg
ctx2bwdarity ∅        = []
ctx2bwdarity (Γ ,⋆ J) = ctx2bwdarity Γ :< Type
ctx2bwdarity (Γ , A)  = ctx2bwdarity Γ :< Term

arity : Builtin → Arity
arity b = ctx2bwdarity (proj₁ (proj₂ (sig b))) <>> []
\end{code}
