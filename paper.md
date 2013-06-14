% An EDSL approach to High Performance Haskell programming
% Johan Ankner; Josef Svenningsson
%

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

The basic unit of a meta-repa program are values of the type `Expr a`,
which represent expressions in the core language. For example, a
simple numeric expression can be written using the standard Num
instance:

~~~ {.haskell}
e :: Expr Int
e = 10*5+2
~~~

Functions are written as normal Haskell functions over `Expr`s. For
example, a simple numeric function:

~~~ {.haskell}
f :: Expr Int -> Expr Int -> Expr Int
f a b = a*a - b
~~~

Core language constructs:

~~~ {.haskell}

if_ :: Computable a => Expr Bool -> a -> a -> a

iterateWhile :: Computable a 
						 => (a -> Expr Bool)
						 -> (a -> a) 
						 -> a
						 -> a

~~~

Note that polymorphic arguments use the class `Computable` rather
than being of type `Expr a`. The `Computable` class allows the
programmer to write code as if certain Haskell constructs are part of
the core language. For example, it is more convenient to work with the
type `(Expr Int, Expr Double)` rather than `Expr (Int, Double)`
because the former can be constructed and deconstructed with Haskell's
ordinary tuple syntax. `Computable` handles the tupling and untupling
in the core language automatically.

\TODO{Say something about Shapes}
\TODO{Explain that there are two types of arrays, Pull and Push}

The library for array computations are implemented as a shallow
embedding on top of the core language.  There are two different types
of arrays in meta-repa, Pull arrays and Push arrays. Pull arrays are
implemented as follows:

~~~ {.haskell}

data Pull sh a = Pull (Shape sh -> a) (Shape sh)

~~~

The first argument to the constructor is a function that takes an
index and computes the element at that index.

\TODO{Discuss library functions for working with arrays}



\TODO{Compare to programming in repa}
\TODO{Mandelbrot as an example}

\begin{figure*}
\begin{tabular}{p{\columnwidth} | p{\columnwidth}}

\begin{verbatim}
type Complex = (Double, Double)
type ComplexPlane r = Array r DIM2 Complex
type StepPlane r = Array r DIM2 (Complex,Int)

step :: ComplexPlane U 
		 -> StepPlane U
		 -> IO (StepPlane U)
step cs zs = computeP $ zipWith stepPoint cs zs
  where
    stepPoint 
			:: Complex -> (Complex,Int) -> (Complex,Int)
    {-# INLINE stepPoint #-}
    stepPoint !c (!z,!i) =
        if magnitude z' > 4.0 
					then (z,i)
					else (z',i+1)
      where
        z' = next c z
    next :: Complex -> Complex -> Complex
    {-# INLINE next #-}
    next !c !z = c + (z * z)
\end{verbatim}
&
\begin{verbatim}
type Complex = (Expr Double, Expr Double)
type ComplexPlane = Pull DIM2 Complex
type StepPlane = Pull DIM2 (Complex, Expr Int)

step :: ComplexPlane 
		 -> StepPlane
		 -> StepPlane
step cs zs = forcePull $ zipWith stepPoint cs zs
  where
    stepPoint :: Complex 
							-> (Complex,Expr Int)
							-> (Complex,Expr Int)
    stepPoint c (z,i) =
        if_ (magnitude z' > 4.0)
          (z,i)
          (z',i+1)
      where
        z' = next c z
    next :: Complex -> Complex -> Complex
    next c z = c + (z * z)
\end{verbatim}
\end{tabular}
\label{fig:comparison}
\caption{A comparison between programming in repa and meta-repa}
\end{figure*}

Differences:

* Int, Double -> Expr Int, Expr Double
* No explicitily manifest array type.
* if then else -> if_
* Bang patterns and INLINE


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

  Fusion can easily be prevented by inserting the function `force`,
  which will store the array to memory. This follows the design used in
  Feldspar and repa.

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

\begin{figure*}[t]
\center
\includegraphics[scale=0.8]{meta.pdf}
\label{fig:pipeline}
\caption{The compilation pipeline for meta-repa programs}
\end{figure*}

This section explains most of the implementation of meta-repa. Some
details, in particular the use of push arrays are explained in section
\ref{sec:push}.

Programs in meta-repa are in fact program generators. When meta-repa
functions are run they produce abstract syntax trees which are then
further translated and transformed. Figure \ref{fig:pipeline} gives an
overview of this process. Boxes represents abstract syntax trees and
circles represents transformations and translations. First the code
within the Template Haskell splice is run. This will compute a term of
type `Expr`, a GADT which ensures type safety of program by
construction. Since all functions defined using the meta-repa library
are really Haskell functions they will simply compute new syntax
trees. The net effect will be that the code in the functions will be
inlined (unless prevented by the programmer). The inlining happens
purely as a result of Haskell's evalutation, there is no code in the
meta-repa library which performs any inlining.

The type `Expr` uses higher order abstract syntax to represent
programs. This representation is convenient for programming with but
somewhat less ideal for rewriting programs. The AST is therefore
converted into a first order representation, which we will refer to as
`FOAS`.

Two optimizations are performed on the `FOAS` representation: common
subexpression elimination (CSE) and code motion. CSE finds identical
subexpressions and shares their computation so that it only happens
once. The transformation is selective with exactly what subexpressions
to share. We found in measurements that the generated code was faster
if most small subexpressions were left unshared. The code motion
transformation moves code up the syntax tree to that it is computed as
early as possible. It also moves constant expressions out of loops.

Another popular way to achieve CSE is to represent the syntax tree as
a dag and thereby get sharing as a side effect of the
representation. We've chosed to use a simpler tree based
representation with an explicit `let` construct for sharing which
allows us to make local decisions about what to share and what to
duplicate.

Once the `FOAS` representation is transformed a wrapper is added
around it which gives the expression a type which makes it easier to
call from Haskell. Then, as a last step the code is translated to
Template Haskell and spliced into the module by GHC.

We would like to point out that the use of Template Haskell is just a
matter of convenience and not of fundamental importance. Another
possibility would have been to write the generated Haskell code to a
separate file which the programmer would then have to compile
separately.

## Core Language(s)

\begin{figure}
\begin{verbatim}
data Expr a where
  Var   :: Int -> Expr a
  Value :: a -> Expr a

  Lambda :: Type a -> (Expr a -> Expr b)
         -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b

  Binop :: Binop a -> Expr a -> Expr a -> Expr a
  Equal :: Eq a => Expr a -> Expr a -> Expr Bool
  NotEqual :: Eq a => Expr a -> Expr a -> Expr Bool
  Unit :: Expr ()

  Tup2 :: Expr a -> Expr b -> Expr (a,b)
  Fst :: Expr (a,b) -> Expr a
  Snd :: Expr (a,b) -> Expr b

  Let :: Expr a -> (Expr a -> Expr b) -> Expr b
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  IterateWhile :: Expr (s -> Bool) -> Expr (s -> s)
               -> Expr s -> Expr s

  ReadIArray :: Storable a =>
                Expr (UArray Int a) -> Expr Int
             -> Expr a
  ArrayLength :: Storable a =>
                 Expr (UArray Int a) -> Expr Int

  Return :: Expr a -> Expr (IO a)
  Bind   :: Expr (IO a) -> Expr (a -> IO b)
         -> Expr (IO b)

  WhileM :: Expr (s -> Bool) -> Expr (s -> s)
         -> Expr (s -> IO ()) -> Expr s
         -> Expr (IO ())

  RunMutableArray :: Storable a =>
                     Expr (IO (IOUArray Int a))
                  -> Expr (UArray Int a)

  NewArray   :: Storable a =>
                Type a -> Expr Int
             -> Expr (IO (IOUArray Int a))
  ReadArray  :: Storable a =>
                Expr (IOUArray Int a) -> Expr Int
             -> Expr (IO a)
  WriteArray :: Storable a =>
                Expr (IOUArray Int a)
             -> Expr Int -> Expr a -> Expr (IO ())
  ParM       :: Expr Int -> Expr (Int -> IO ())
             -> Expr (IO ())
\end{verbatim}
\label{fig:expr}
\caption{A representative subset of the \texttt{Expr} data type}
\end{figure}

The core language of meta-repa, represented by the `Expr` data type,
is a standard typed higher order abstract syntax representation
implemented using GADTs. A fragment with the most relevant construcs
is shown in picture \ref{fig:expr}.

The first two constructors `Var` and `Value` are used for compilation
and evaluation and will never be present in trees produced by code
written in meta-repa.

The constructors `Lambda` and `App` together with the constructs for
binary operators and comparisons form a small functional language for
arithmetic and tests which is useful for scalar computations.

The `Let` construct is used for explicit sharing in the syntax
tree. It is exposed to the programmer via the `let_` function which
can be used to prevent inlining and explicitly share computations. It
is worth pointing out that meta-repa does not employ observable
sharing [@claessen1999observable] so no new `Let` constructors are
introduced when the program is represented by the `Expr` type.

The constructor `If` and `IterateWhile` are unsurprising constrol flow
constructs for pure computations.

There are two types of in-memory arrays in the code language. These
arrays are not exposed to the programmer, they are instead used as
building blocks for the array types implemented as shallow embedding
which we explain in the next subsection. The two type of array in the
core language are always allocated in memory. One of the types is
`UArray` which represents pure arrays and the constructs `ReadIArray`
and `ArrayLength` can be used to query them. There is no pure
construct for creating pure arrays, instead they must be created
through destructive, monadic constructs which we turn to next.

To begin with, the monadic construct contains the generic `Return`,
`Bind` and a looping construct, `WhileM`. Then there are a few
constructs for creating, reading and updating mutable arrays. The
`RunMutableArray` construct takes a monadic computation and returns a
pure array. In that way it is similar to the ST monad
[@launchbury1994lazy]. Compared to the state monad, the state
parameter in the type has been omitted, since there is no construct
corresponding to `runST` with a polymorphic return type.

Finally, there is the parallel for-loop, `ParM`, which is the
construct for parallel computations. 

There are a couple of things to note about the core language:

* It is monomorphic. Having a monomorphic language is important to be
  able to always generate unboxed Haskell code. 

  Having a monomorphic core language does not stop the programmer from
  writing polymorpic programs or using overloading. Functions can be
  written which work for several different base types. The only
  restriction is that when compiling a meta-repa program, all types
  must be instantiated to monomorphic types.

\TODO{Describe the monad and what operations it provides}
\TODO{Performance guarantees}

\TODO{No observable sharing}

\TODO{Evaluation function}

The core language comes with an evaluation function which defined the
semantics. The evaluation function is straighforward to write. It is
also very useful for trying out the language during its development
and as a reference semantics to test the Template Haskell against.

## Shallow Embeddings for Arrays
\label{sec:shallow}

The implementation of meta-repa follows the methodology of combining
deep and shallow embeddings descibed in
[@svenningsson12:DeepShallow]. The type `Expr` is a deeply embedded
core language which contains all the language constructs necessary to
generate efficient code from any meta-repa program. On top of the core
language there are several shallow embeddings; in the case of
meta-repa there are two types of arrays which are implemented as
shallow embeddings. Implementing language constructs as shallow
embeddings help keep the core language small and \TODO{fusion}

\TODO{Pull arrays}
\TODO{Fusion for free}

## From type level to value level programming

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

## Runtime system

The library meta-repa relies on a small runtime system in order to set
up the parallel computations and distribute them over the available
processes. We have chosen to reuse the runtime system from repa by
simply calling the the appropriate low-level functions provided the
library. This has had a number of positive effects. First, the repa
library has a well-developed and mature runtime system which has been
used and improved over several years. Secondly, when doing
measurements to compare against repa, the runtime system is equal and
eliminates a lot of possible sources of differences which could affect
the benchmarks. Being able to share runtime system means that the
comparisons can focus on the generated code and the amount of
parallelism made available in the different libraries.

A downside to using the runtime system of repa is that it only handles
flat parallelism. This means that it is not possible to nest the
`ParM` construct in the core language and some care has gone into
designing the API in meta-repa to avoid nesting. However, nested
parallelism is a much harder problem than flat parallelism and
developing a completely new runtime for meta-repa would have outside
the scope of the project.

# Push arrays
\label{sec:push}

The programmers interface in meta-repa is heavily inspired by repa,
but some things have been consiously made different. The most
significant divergence is the choice of having two kinds of arrays.

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

Giving a full account of push arrays falls outside the scope of this
paper. The interested reader is refered to [@claessen2012expressive]
where push arrays were introduced. However, we will present enough
detail to get an appreciation for why they are useful for the purpose
of high performance Haskell programming.

Push arrays are implemented as follows:

~~~ {.haskell}
data Push sh a = 
  Push ((Shape sh -> a -> M ()) -> M ()) (Shape sh)
~~~

The second argument to the `Push` constructor is the extent of the
array. The first argument is a monadic computation which, when run,
will write the array to memory. We refer to this computation as the
kernel of the array. The kernel is parameterized by the operation used
to write to memory. Parameterizing over the writing operation is what
gives push arrays their flexibility.

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

force :: Storable a =>
         Push sh (Expr a)
      -> Push sh (Expr a)
force (Push f l) = Push f' l
  where f' k =
    do arr <- newArrayE (size l)
       f (\sh a -> writeArrayE arr (toIndex l sh) a)
       forShape l $ \i -> do
         a <- readArrayE arr i
         k (fromIndex l i) a
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
better implementation of concatenation would have used parallel
composition instead of sequential composition. However, our current
runtime system doesn't support that. There is still room for
improvements.

Finally, the function `force` show how push arrays are written to
memory. It can be used by the programmer to prevent fusion and make
sure that an array is written to memory. The kernel of the resulting
array allocates a new in-memory array and calls the kernel of the
input array with a write function as a parameter which writes all the
elements to the newly allocated array. It then iterates through the
array in memory writing all the elements using the write function
parameter.

## FFT
\label{sec:fft}

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

~~~
stencilSobel :: Stencil DIM2 (Expr Float) (Expr Float)
stencilSobel = [stencilM| -1 0 1
                          -2 0 2
                          -1 0 1 |]

stencilBlur :: Stencil DIM2 (Expr Float) (Expr Float)
stencilBlur = [stencilM| 2  4  5  4  2
                         4  9 12  9  4
                         5 12 15 12  5
                         4  9 12  9  4
                         2  4  5  4  2 |]
~~~

# Measurements
\label{sec:benchmarks}

\begin{table*}[t]
\center
\begin{tabular}{|l||l|l||l|l||l|l|}
\hline
 & \multicolumn{2}{c||}{no \tt -threaded} & \multicolumn{2}{c||}{\tt -N1} & \multicolumn{2}{c|}{\tt -N4}\\
\hline
\bf benchmark & \bf meta  & \bf repa  & \bf meta  & \bf repa  & \bf meta  & \bf repa \\
\hline
\hline
matrix100 & 708.5684 us & 986.2883 us & 817.5255 us & 1.164313 ms & 252.8681 us & 470.2105 us \\
\hline
matrix500 & 85.49098 ms & 92.95946 ms &  85.75845 ms &  93.28728 ms & 21.77168 ms & 23.77432 ms \\
\hline
matrix1000 & 706.9690 ms & 739.6758 ms & 708.3879 ms & 741.0649 ms & 179.1465 ms & 189.0575 ms \\
\hline
blur & 327.2941 ms & 318.8542 ms & 327.8346 ms & 348.8333 ms & 83.81088 ms & 108.0091 ms \\
\hline
sobel & 72.23000 ms & 52.17829 ms & 72.99609 ms & 54.15539 ms & 19.64092 ms & 17.28642 ms \\
\hline
fft-small & 3.437408 ms & 13.49565 ms &  3.824134 ms & 147.7129 ms & 1.382004 ms & 190.7312 ms \\
\hline
fft-medium & 15.51583 ms & 57.02767 ms & 16.79921 ms & 589.5525 ms & 5.415479 ms & 767.9146 ms \\
\hline
fft-large & 32.99549 ms & 117.4556 ms & 36.49858 ms & 1.185318 s & 11.14325 ms & 1.532703 s \\
\hline
\end{tabular}
\label{tab:benchmarks}
\caption{Performance measurements comparing meta-repa with repa}
\end{table*}

Table \ref{tab:benchmarks} presents benchmarks comparing the
performance of meta-repa with repa. All measurements have been
performed on a machine with a four core Intel Core i7-3770K processor
clocked at 3.5 GHz and with 16 Gb of RAM, running Ubuntu 12.04.1. All
programs have been compiled with GHC version 7.6.1 and LLVM version
2.9. The package criterion [@criterion] has been used to perform the
measurements and the times in the table are the mean times reported by
criterion. The version of the repa library and the repa-algorithms
library is 3.2.3.1. All benchmarks was compiled with the flags
recommended by the repa documentation: `-Odph` `-rtsopts` `-threaded`
`-fno-liberate-case` \newline `-funfolding-use-threshold1000` \newline
`-funfolding-keeness-factor1000` `-fllvm` `-optlo-O3`.

The measurements are divided into three different categories: "no
`-threaded`", "`-N1`" and "`-N4`". The category "no `-threaded`" means
that the flag `-threaded` was left out when compiling the benchmarks,
making them run without the parallel runtime system. The main reason
for including this category is the fft benchmarks which we discuss
below. The categories "`-N1`" and "`-N4`" means that the benchmarks
where run with the corresponding runtime flag to indicate how many
processes they should be run with. The "`-N1`" category only uses one
process but gives an indication of the penalty of running with the
parallel runtime system compared to the "no `-threaded`" category.

The benchmarks matrix100, matrix500 and matrix1000 are matrix
multiplication of two matrices of sizes $100 \times 100$, $500 \times
500$ and $1000 \times 1000$ respectively. Blur and sobel are two
stencil operations on two-dimensional images. The blur stencil has
size $5 \times 5$ while the sobel stencil is $3 \times 3$. Both
benchmarks have been run on a png image of size $3000 \times 2400$
with eight bit color depth. Finally, the benchmarks fft-small, -medium
and -large runs FFT on a randomized, complex valued, one-dimensional
array of length $2^{16}$, $2^{17}$ and $2^{18}$ respectively.

In the matrix multiplication benchmarks meta-repa has a small but
consistent advantage over repa. Both implementations scales well to
four cores. The blur benchmark exhibits a peculiar behaviour. Without
the `-threaded` flag the repa library has a slight advantage while the
reverse is true when using the parallel runtime system. For the sobel
benchmark the repa library is consistently ahead. The FFT benchmarks
seem to exhibit a bug in the repa library. When compiled using the
parallel runtime it shows really poor performance. For this reason we
included the sequential benchmark which shows more reasonable running
times. However, meta-repa still outperforms repa by almost a factor of
four due to the use of push arrays, as explained in section
\ref{sec:fft}.

The conclusion we draw from these measurements is that the methodology
we've used when developing meta-repa is very fruitful. Our library is
on par with, and in some cases even beats repa, which is a mature
library for high performance computing developed over several years.

# Discussion

This paper presents a methodology for using embedded domain specific
langauges to implement high performance Haskell programs. It comes
with a complete example which shows the viablity of the approach.

## Summary

Out methodology has some pros and cons which we have mentioned
thoughout the paper. Here is a concise summary of the different
aspects of using the embedded langauge approach.

Advantages:

* More natural programming model.

  Since the embedded language can be given a semantics which matches
  the particular implementation

* Performance guarantees

  When using a restricted language, the problem of optimizing it
  becomes much simpler, to the point where it is possible to give
  strong performance guarantees.

* Inlining and fusion for free

  

* Domain specific optimizations

  Even though we generate Haskell code and make use of the
  optimizations provided by GHC and whichever backend it uses, it is
  possible to write domain specific optimizations

* Value level programming instead of type level programming

  As seen in section \ref{sec:shape}, the implementation can become
  simpler by moving type level computations to the value level.  This
  helps to simplify types in the API and makes the library easier to
  use and comprehend.

Drawbacks:

* Having to implement an embedded language requires an upfront
  investment in defining the language and its compilation.

* Some types become a little more awkward. For instance, one has to
  write `Expr Int` instead of `Int`.

Weighing the advantages and drawbacks against each other our
conclusion is clear. Using the embedded language approach is a net win
in the long term for writing high performance application in Haskell.

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

\acks

The FFT implementation is a port from a Feldspar implementation
written by Anders Persson. Thanks to Emil Axelsson for suggesting to
use GADTs to represent the `Shape` type.

# References
