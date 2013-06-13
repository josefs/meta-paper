% An EDSL approach to High Performance Haskell programming
% Johan Ankner; Josef Svenningsson
%

# Abstract

\TODO{Abstract}

# Introduction

In recent years the Haskell community has developed an increasing
interest in writing programs which perform well. Libraries have been developed 

Even though GHC is a terrific optimizing compiler, by necessity it sometimes fall short of generating the code that 



Contributions:

* We present a new methodology for writing high performance Haskell
  programs. We argue for using an embedded domain specific language
  and generate Haskell from that language. Programming in the domain
  specific language will be easier for the end user because the
  language can be given a semantics which matches the problem domain.

  Furthermore, several aspects of the implementation of the library
  becomes simpler when using the embedded language approach. In
  particular, many things that are done of the type level can now be
  done the value level.

* We show how we use the technique of combining deep and shallow
  embeddings, building on the work in [@svenningsson12:DeepShallow],
  to implement arrays. This technique helps limit the size of the core
  language, implement fusion for arrays for free and give strong
  optimization guarantees.

* We demonstrate a complete case-study, meta-repa, showing the
  benefits of our approach. It is a reimplementation of the repa
  [@keller2010regular] library using the embedded language
  approach. We explain the implementation in section
  \ref{sec:impl}. Section \ref{sec:benchmarks} presents benchmarks
  showing that meta-repa is as fast, or faster, than repa.

* Instead of one array type we have two. We have included push arrays
  [@claessen2012expressive] in our implementation. The result is a
  vastly simpler library and although the user of our library must now
  use two different types of arrays we consider the resulting API to
  be easier to use. We explain the details in section \ref{sec:push}.

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

* *Every function is inlined by default*. 

  In high performance code, inlining functions is the right default
  behaviour and generally increases the performance a lot. When the
  programmer wants to prevent inlining, for whatever reason, it is
  simple to create a locally defined function with the `let_`
  combinator provided by our library.

* *Operations on arrays are fused automatically*.

  Our library has two types of arrays, `Pull` and `Push`, and all
  operations working on only one of these types will always be fused,
  as will conversions from `Pull` to `Push`. However, conversions from
  `Push` to `Pull` are not fused. This exception might seem surprising
  but we explain why this is the right default in section
  \ref{sec:push} on Push arrays.

  Fusion can easily be prevented 

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

  Having a monomorphic core language does not stop the programmer from
  writing polymorpic programs or using overloading. Functions can be
  written which work for several different base types. The only
  restriction is that when compiling a meta-repa program, all types
  must be instantiated to monomorphic types.

After the HOAS representation is generated, it is translated to a
first order representation. This representation is used for performing
domain specific optimizations. We have implemented two optimizations:
common subexpression elimination and loop-invariant code motion.

\TODO{Describe the monad and what operations it provides}
\TODO{Performance guarantees}
\TODO{Inlining for free}
\TODO{Domain specific optimizations, CSE and loop-invariant code motion.}

\TODO{No observable sharing}

## Shallow Embeddings for Arrays
\label{sec:shallow}

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
  (:.) :: Shape sh -> Expr Length
       -> Shape (sh :. Expr Length)
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
toIndex (sh1 :. sh2) (i1 :. i2)
  = toIndex sh1 i1 * sh2 + i2

intersectDim :: Shape sh -> Shape sh -> Shape sh
intersectDim Z Z = Z
intersectDim (sh1 :. n1) (sh2 :. n2)
  = (intersectDim sh1 sh2 :. (min n1 n2))

inRange :: Shape sh -> Shape sh -> Shape sh
        -> Expr Bool
inRange Z Z Z = true
inRange (shL :. l) (shU :. u) (sh :. i)
  = l <= i && i < u && inRange shL shU sh
~~~

\TODO{What Shape functions are interesting to show}

There are still some functions on `Shape` which require a type class
to be implemented. These are the functions which doesn't take any
arguments of type `Shape` but whose result are of type `Shape`.

~~~{.haskell}
class Shapely sh where
  mkShape :: Expr Index -> Shape sh
  toShape :: Int -> Expr (UArray Int Length)
          -> Shape sh

instance Shapely Z where
  mkShape _ = Z
  toShape _ _ = Z

instance Shapely sh => Shapely (sh :. Expr Length)
    where
  mkShape i = mkShape i :. i
  toShape i arr
    = toShape (i+1) arr :.
      (readIArray arr (P.fromIntegral i))
~~~


## Compilation to Haskell

\TODO{Show how some language features get compiled very easily to
Haskell using quotation}
\TODO{Explain the use of Template Haskell and that it is optional.}

# Push arrays
\label{sec:push}

The interface meta-repa is heavily inspired by repa, but some things
have been consiously made different. The most significant divergence
is the choice of having two kinds of arrays.

In meta-repa there are two types of arrays, delayed arrays. One of
these types, pull arrays, were already presented in section
\ref{sec:shallow}. The other type is push arrays, a notion originally
introduced in [@claessen2012expressive]. Push arrays shares many
significant properties with pull arrays: they can be fused just as
easily, are efficiently parallelizeable, and have a `Functor`
instance.

However, push arrays are also in many ways complementary to pull
arrays. The two types have different strengths:

* Pull arrays can be indexed efficiently and by extension
  can also be decomposed into subarrays. Pull arrays also supports
  pointwise zipping.

* Push arrays can efficiently be concatenated. Futhermore, they allow
  sharing computations between different array elements and generating
  code which writes multiple array elements per loop interation.

It's worth noting that both pull- and push arrays can be altered to
efficiently support some of the features that they lack, when defined
in their simplest form. However, based on our experience, these
alterations lose the strong optimization guarantees; either fusion is
lost, or sometime the generated code is slow.  In meta-repa we have
specifically chosen to keep the implementation of the arrays simple
and to provide strong guarantees towards the programmer about what
optimizations can be expected.

Push arrays are implemented as follows:

~~~ {.haskell}
data Push sh a = 
  Push ((Shape sh -> a -> M ()) -> M ()) (Shape sh)
~~~

The second argument to the `Push` constructor is the extent of the
array. The first argument is a monadic computation which will write an
array to memory. We refer to this computation as the kernel of the
array. The kernel is parameterized by the operation used to write
to memory. Parameterizing over the writing operation is what gives
push arrays their flexibility.

Here are some example functions on push arrays.

~~~ {.haskell}
enumFromTo :: Expr Int -> Expr Int
           -> Push (Z :. Expr Int) (Expr Int)
enumFromTo f t = Push loop (Z :. t - f + 1)
  where loop w = parM (t - f + 1) (\i ->
  	       	   w (Z :. i) (i + f)
                 )

instance Functor (Push sh) where
  fmap f (Push m sh) = Push n sh
    where n w = m (\i a -> w i (f a))

(+.+) :: Push (sh:.Expr Length) a
      -> Push (sh:.Expr Length) a
      -> Push (sh:.Expr Length) a
(Push m1 (sh1:.l1)) +.+ (Push m2 (sh2:.l2))
    = Push m (sh1:.(l1 + l2))
  where m k = m1 k >>
              m2 (\(sh:.i) a -> k (sh:.(i+l1)) a)
~~~

The function `enumFromTo` is similar to the standard Haskell function
on lists with the same name. The kernel `loop` is defined in terms of
a parallel for-loop which writes each element.

Just like pull arrays, push arrays can be made an instance of the type
class `Functor` as shown in the code above. The kernel of the result
array simply calls the kernel of the argument array but modifies the
write function such that the elements get transformed before being
written.

The operator `+.+` is a good example of the benefits with push
arrays. It defines concatenation along the final dimension of two push
arrays. (The arrays must have the same size in all the other
dimensions, something which is not checked.) The kernel of the
resulting push array is simply sequential composition of the kernels
for the two argument arrays. In the common case this will mean that
the final array is written using two loops, each writing to its own
part of the array. This should be compared the code generated from
concatenation on pull array, which is a single loop containing a
branch which checks which argument array to read from. Concatenation
for push arrays has effectively moved the conditional out of the loop,
a big win in term of performance. It should be added that an even
better implementation of concatenation would have been used parallel
composition instead of sequential composition. However, our current
runtime system doesn't support that. There is still room for
improvements.

## FFT

One example algorithm where push arrays have proven valueable is FFT.
Below are the relevant bits of our implementation, gently beautified
for presentation purposes.

~~~
fft :: (Computable a, Num a, Storable (Internal a))
    => Pull DIM1 a -> Pull DIM1 a -> Pull DIM1 a
fft ws vs = forLoop (ilog2 $ length1 vs) vs stage
  where
    stage s xs = freezeToVector $
        chnk (arrayLength xs .>>. s)
             (butterfly (ixMap (.<<. s) ws)) xs

butterfly :: (Computable a, Num a)
          => Pull DIM1 a -> Pull DIM1 a
          -> Push DIM1 a
butterfly ws vs = unhalve $ toPushS $ 
                  zipWith3 dft2 ws ys zs
  where
    (ys,zs) = halve vs

unhalve :: (Computable a)
        => Push DIM1 (a,a) -> Push DIM1 a
unhalve (Push f (Z :. l))
    = Push (f . spread) (Z :. l * 2)
  where spread f (Z :. ix) (a1,a2)
          = f (Z :. ix) a1 >> f (Z :. (ix+l)) a2
~~~

The function `fft` is a Cooley-Tukey radix-2 decimation in frequency
algorithm. There are many details here which are not important for the
purpose of the current discussion and so we leave them out. The
essential point is the function `unhalve` which is used to implement
the butterfly network. It takes a push array of pairs and flattens it
such that the first half of the resulting push array contains all the
first components of the pairs and the second half the second
components. The crucial bit is that the computation of the pair can be
shared and that the two components can be written in the same loop
iteration. It is not possible to express this kind of sharing using
pull arrays alone.

In section \ref{sec:benchmarks} we present benchmarks showing that
using push arrays translates to a real speed advantage.


## Stencil computations

\TODO{Explain how to write efficient stencil computations}
\TODO{Pull/Push a nice model for Stencils}

# Measurements
\label{sec:benchmarks}

\TODO{Matrix Multiplication}
\TODO{Sobel filter}
\TODO{Blur}
\TODO{FFT}
\TODO{(Maybe scale up)}

# Discussion

## Summary

Pros

* More natural code
* Performance guarantees
* Inlining and fusion for free
* Domain specific optimizations
* Value level programming instead of type level programming

Cons

* Having to implement an embedded language requires extra code

* Some types become a little more awkward. For instance one has to
  write `Expr Int` instead of `Int`.

## Future work

The library meta-repa is only the first step towards a framework for
writing high performance Haskell programs. We hope to be able to build
on meta-repa to support application areas different from parallel
array programming. However, it is not clear that there will be one
single language which will be able to handle all use cases. It might
be that new languages have to be developed to cater for needs in
different domains. Further research is needed to determine the best
way to use the methodology presented in this paper.

# Related work


\TODO{Related work}
\TODO{Nikola}
\TODO{Repa}
\TODO{Deep vs Shallow}
\TODO{Little languages in LISP}

Domain specific languages have become increasingly popular over the
last decade, although they have a long and rich history
[@Bentley:1986:PPL:6424.315691].

Haskell has proved very effective as a host language for *embedding*
domain specific languages.

One particular feature of the methodology presented in this paper is
that it enables library writers to easily write their own
optimizations so that they can be sure to get the performance they
want. Recently GHC has been equipped with a plugin mechanism which
allows for easily adding new optimization passes. While the plugin
facility is very useful it will have a hard time providing the kind of
performance guarantees which our library offers. The reason is because
it is compiling all of Haskell and due to Haskell's generality,
providing the the same kind of performance guarantees is an
undecidable problem. Again, formulating a limited, domain specific
language pays off by making the problem of optimization feasible.

Henning Thielemann has developed a family of libraries for audio
processing, called "synthesizer" [@synthesizer]. One particular member
in this family is "synthesizer-llvm" [@synthesizer-llvm] which employs
runtime code generation using LLVM to achieve high performance. This
methodology is similar in spirit to our approach but we use
compile-time code generation and generate Haskell. For our purposes,
generating Haskell was sufficient from a performance perspective and
very convenient as Haskell allows us to generate relatively high
level code compared to LLVM.

The general design philosophy seems to be similar to ours.

The guarantee of fusion for arrays in meta-repa is the same as in
Feldspar [@axelsson2011design] and repa. It stems from the
implementation technique pioneered by Feldspar.

Push arrays were first introduced in Obsidian
[@claessen2012expressive] and has subsequently been implemented in
Feldspar [@axelsson2011design] and Nikola [@mainland2010nikola].

# Acknowledgements

The FFT implementation is a port from a Feldspar implementation
written by Anders Persson.

# References
