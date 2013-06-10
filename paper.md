% An EDSL approach to High Performance Haskell programming
% Johan Ankner; Josef Svenningsson
%

# Abstract

\TODO{Abstract}

# Introduction

In recent years the Haskell community has developed an increasing
interest in writing programs that perform well.


Contributions:

* We present a new methodology for writing high performance Haskell
  programs. We argue for using an embedded domain specific language
  and generate Haskell from that language. The resulting code will be
  easier to write for the end user because the domain specific
  language can be tailored to the 

  Furthermore, several aspects of the implementation of the library
  becomes simpler when using the embedded language approach. In
  particular, many things that are done of the type level can now be
  done the value level.

* We show how we use the technique of combining deep and shallow
  embeddings, building on the work in [@DeepShallow], to implement
  arrays. This technique helps limit the size of the core language,
  implement fusion for arrays for free and give strong optimization
  guarantees.

* We demonstrate a complete case-study, meta-repa, showing the
  benefits of our approach. It is a reimplementation of the repa
  library using the embedded language approach. We explain the
  implementation in section \ref{sec:impl}. Section
  \ref{sec:benchmarks} presents benchmarks showing that meta-repa is
  as fast, or faster, than repa.

* Instead of one array type we have two. We have included push arrays
  [@pusharrays] in our implementation. The result is a vastly simpler
  library and although the user of our library must now use two
  different types of arrays we consider the resulting API to be easier
  to use. We explain the details in section \ref{sec:push}.

# Programming in meta-repa

\TODO{Compare to programming in repa}
\TODO{Mandelbrot as an example}

\TODO{Explain that there are two types of arrays, Pull and Push}
\TODO{Make sure to point out the Expr type and the class Computable}
\TODO{Also mention the use of Template Haskell}

## Contract towards the programmer

The library meta-repa comes with a set of guarantees towards the
programmer. These contracts helps the programmer understand the
efficiency of a particular program. They also how precisely when a
programmer can introduce abstraction without losing any performance.

* *All types are monomorphised and unboxed*.

  In particular, expressions of type `Expr Int` will be compiled to
  `Int#`, `Expr Float` to `Float#`, pairs to unboxed pairs and so
  forth.

  The programmer is free to write polymorphic and overloaded code. But once
  the final Haskell code is generated, all types will be monomorphic and
  unboxed.

* *Every function is inlined by default*. \TODO{Explain more, and how to
  prevent inlining}

* *Operations on arrays are fused automatically*. \TODO{Explain the situation with Pull and Push arrays}

* *Common subexpression elimination and code motion are applied
  extensively on the program*.
 
  GHC already does these optimizations to some extent but because of
  the domain specific nature of our library, we can apply these
  optimizations more extensively than GHC.

These guarantees and optimizations are possible and practical because
we are working with a limited domain specific language. When compiling
a general purpose language, many program optimizations often turn out
to be pessimizations for certain classes of programs. By constructing
a smaller language we've made the problem of optimizing programs much
easier.

In the next sections we will describe how our implementation achieves
these guarantees. Many of them come for free, as a side effect of how
we're representing programs.

# Implementation of meta-repa
\label{sec:impl}

Programs in meta-repa are in fact program generators. The type
`Expr` is the type of abstract syntax trees for meta-repa
programs. Similarly, the class `Computable` denotes all types which
can be translated into `Expr`.


We would like to point out that the use of Template Haskell is just a
matter of convenience and not of fundamental importance. Another
possibility would have been to write the generated Haskell code to a
separate file which the programmer would then have to compile
separately.

The implementation of meta-repa follows the methodology of combining
deep and shallow embeddings descibed in
[@svenningsson12:DeepShallow]. It has a deeply embedded core language
which contains all the language constructs necessary to generate
efficient code from any program. On top of the core language there are
several shallow embeddings, in the case of meta-repa there are two
types of arrays which are implemented as shallow
embeddings. Implementing language constructs as shallow
embeddings help keep the core language small and \TODO{fusion}

\TODO{Generating the AST}
\TODO{Inlining by default}


## Core Language(s)

The core language of meta-repa is a standard typed higher order
abstract syntax representation implemented using GADTs. A fragment
with the most relevant constructs is shown in picture \ref{}.

~~~ {.haskell}
data Expr a where
  Var   :: Int -> Expr a
  Var2  :: Name -> Expr a
  Value :: a -> Expr a

  Lambda :: Type a -> (Expr a -> Expr b) -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b

  Binop :: Binop a -> Expr a -> Expr a -> Expr a
  Abs :: Num a => Expr a -> Expr a
  Signum :: Num a => Expr a -> Expr a
  Recip :: Fractional a => Expr a -> Expr a
  FromInteger :: Num a => TypeConst a -> Integer -> Expr a
  FromRational :: Fractional a => TypeConst a -> Rational -> Expr a
  FromIntegral :: (Integral a, Num b) => Type b -> Expr a -> Expr b
  Complement :: Bits a => Expr a -> Expr a
  Bit :: Bits a => Expr Int -> Expr a
  Rotate :: Bits a => Expr a -> Expr Int -> Expr a
  ShiftL :: Bits a => Expr a -> Expr Int -> Expr a
  ShiftR :: Bits a => Expr a -> Expr Int -> Expr a

  BoolLit :: Bool -> Expr Bool

  Equal :: Eq a => Expr a -> Expr a -> Expr Bool
  NotEqual :: Eq a => Expr a -> Expr a -> Expr Bool

  GTH :: Ord a => Expr a -> Expr a -> Expr Bool
  LTH :: Ord a => Expr a -> Expr a -> Expr Bool
  GTE :: Ord a => Expr a -> Expr a -> Expr Bool
  LTE :: Ord a => Expr a -> Expr a -> Expr Bool

  Unit :: Expr ()

  Tup2 :: Expr a -> Expr b -> Expr (a,b)
  Fst :: Expr (a,b) -> Expr a
  Snd :: Expr (a,b) -> Expr b

  TupN :: (Tup t) => t Expr -> Expr (t Id)
  GetN :: (Get n t Expr b) => Int -> n -> Expr (t Id) -> Expr b

  Let :: Expr a -> (Expr a -> Expr b) -> Expr b

  Return :: Expr a -> Expr (IO a)
  Bind   :: Expr (IO a) -> Expr (a -> IO b) -> Expr (IO b)

  If :: Expr Bool -> Expr a -> Expr a -> Expr a

  Rec :: Expr ((a -> r) -> a -> r) -> Expr a -> Expr r
  IterateWhile :: Expr (s -> Bool) -> Expr (s -> s) -> Expr s -> Expr s
  WhileM :: Expr (s -> Bool) -> Expr (s -> s) -> Expr (s -> IO ()) -> Expr s -> Expr (IO ())

  RunMutableArray :: Storable a => Expr (IO (IOUArray Int a)) -> Expr (UArray Int a)
  ReadIArray :: Storable a => Expr (UArray Int a) -> Expr Int -> Expr a
  ArrayLength :: Storable a => Expr (UArray Int a) -> Expr Int

  NewArray   :: Storable a => Type a -> Expr Int -> Expr (IO (IOUArray Int a))
  ReadArray  :: Storable a => Expr (IOUArray Int a) -> Expr Int -> Expr (IO a)
  WriteArray :: Storable a => Expr (IOUArray Int a) -> Expr Int -> Expr a -> Expr (IO ())
  ParM       :: Expr Int -> Expr (Int -> IO ()) -> Expr (IO ())
  Skip       :: Expr (IO ())

  Print :: Show a => Expr a -> Expr (IO ())
~~~
\TODO{Think about how to cut down the fragment}

There are a couple of things to note about the core language:

* It is monomorphic. Having a monomorphic language is important to be
  able to always generate unboxed Haskell code. 

  Having a monomorphic core language does not stop us from writing
  polymorpic programs, using overloading. We can write functions which
  work for several different base types. The only restriction is that
  when compiling a meta-repa program, all types must be instantiated
  to monomorphic types.

After the HOAS representation is generated, it is translated to a
first order representation. This representation is used for performing
domain specific optimizations. We have implemented two optimizations:
common subexpression elimination and loop-invariant code motion.

\TODO{Performance guarantees}
\TODO{Inlining for free}
\TODO{Domain specific optimizations, CSE and loop-invariant code motion.}

\TODO{No observable sharing}

## Shallow Embeddings for Arrays

\TODO{Pull arrays}
\TODO{Fusion for free}

\TODO{The Shape class vs the Shape GADT}

In repa, the type of shapes of an array is represented by a type class
and two singleton (?) types

~~~{.haskell}
class Shape sh where
  ...

data Z = Z
data sh :. e = sh :. e

instance Shape Z where
  ...

instance Shape sh => Shape (sh :. Int) where
  ...
~~~

In meta-repa, thanks to the meta programming approach, shapes can be
represented by an ordinary data type definiton.

~~~{.haskell}
data Z
data sh :. e
data Shape sh where
  Z :: Shape Z
  (:.) :: Shape sh -> Expr Length -> Shape (sh :. Expr Length)
~~~

Defining the `Shape` type like a GADT makes programming with is a
lot more natural. Many of the functions which had to be implemented in
the `Shape` type class in repa can now be implemeted as ordinary
functions.

~~~{.haskell}
dim :: Shape sh -> Int
dim Z = 0
dim (sh :. _) = dim sh + 1

size :: Shape sh -> Expr Length
size Z         = 1
size (sh :. l) = size sh * l

toIndex :: Shape sh -> Shape sh -> Expr Index
toIndex Z _ = 0
toIndex (sh1 :. sh2) (i1 :. i2) = toIndex sh1 i1 * sh2 + i2

intersectDim :: Shape sh -> Shape sh -> Shape sh
intersectDim Z Z = Z
intersectDim (sh1 :. n1) (sh2 :. n2) = (intersectDim sh1 sh2 :. (min n1 n2))

inRange :: Shape sh -> Shape sh -> Shape sh -> Expr Bool
inRange Z Z Z = true
inRange (shL :. l) (shU :. u) (sh :. i) = l <= i && i < u && inRange shL shU sh
~~~

\TODO{What Shape functions are interesting to show}

There are still some functions on `Shape` which require a type class
to be implemented. These are the functions which doesn't take any
arguments of type `Shape` but whose result are of type `Shape`.

~~~{.haskell}
class Shapely sh where
  mkShape :: Expr Index -> Shape sh
  toShape :: Int -> Expr (UArray Int Length) -> Shape sh

instance Shapely Z where
  mkShape _ = Z
  toShape _ _ = Z

instance Shapely sh => Shapely (sh :. Expr Length) where
  mkShape i = mkShape i :. i
  toShape i arr
      = toShape (i+1) arr :. (readIArray arr (P.fromIntegral i))
~~~


## Compilation to Haskell

\TODO{Show how some language features get compiled very easily to
Haskell using quotation}
\TODO{Explain the use of Template Haskell and that it is optional.}

# Push arrays
\label{sec:push}

The interface meta-repa is very closely modelled after repa, but some
things have been consiously made different. The most significant
divergence is the choice of having two kinds of arrays. The repa library contains a refinement of delayed array.

In meta-repa there are two kinds of arrays, delayed arrays, which we
refer to as pull arrays, and push arrays, introduced in
[@claessen2012expressive]. These two kinds of arrays are complementary
and have different strength and weakness.

* Pull arrays can efficiently be indexed 

* Push arrays can efficiently be concatenated and there are 

~~~ {.haskell}
data Push sh a = 
  Push ((Shape sh -> a -> M ()) -> M ()) (Shape sh)
~~~

The second argument to the Push constructor is the extent of the
array. The first argument is a monadic computation which will write an
array to memory. The computation is parameterized by the operation
used to write to memory.

~~~ {.haskell}
enumFromTo :: Expr Int -> Expr Int -> Push (Z :. Expr Int) (Expr Int)
enumFromTo f t = Push loop (Z :. t - f + 1)
  where loop w = parM (t - f + 1) (\i ->
  	       	   w (Z :. i) (i + f)
                 )
~~~

## Stencil computations

# Measurements
\label{sec:benchmarks}

\TODO{Matrix Multiplication}
\TODO{Sobel filter}
\TODO{Blur}
\TODO{FFT}
\TODO{(Maybe scale up)}

# Discussion

## Summary

\TODO{Pros and cons of the embedded language approach}

## Future work

\TODO{Future work: Implementing data structures vs algorithms}

# Related work

\TODO{Related work}
\TODO{Nikola}
\TODO{Repa}
\TODO{Deep vs Shallow}
\TODO{Little languages in LISP}

Push arrays were first introduced in Obsidian
[@claessen2012expressive] and has subsequently been implemented in
Feldspar [@axelsson2011design] and Nikola [@mainland2010nikola].

# References
