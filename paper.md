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
  easier to write for the end user.

* We demonstrate a complete case-study, meta-repa, showing the
  benefits of our approach. We explain the implementation in section
  \ref{sec:impl}.

* Instead of one array type we have two. We have included push arrays
  [@pusharrays] in our implementation. The result is a vastly simpler
  library and although the user of our library must now use two
  different types of arrays we consider the resulting API to be easier
  to use. We explain the details in section \ref{sec:push}.

# Programming in meta-repa

\TODO{Compare to programming in repa}
\TODO{Mandelbrot as an example}

# Implementation of meta-repa
\label{sec:impl}

## Core Language(s)

\TODO{Performance guarantees}
\TODO{Inlining for free}
\TODO{Domain specific optimizations, CSE and loop-invariant code motion.}

\TODO{No observable sharing}

## Shallow Embeddings for Arrays

\TODO{Pull arrays}
\TODO{Fusion for free}

## Compilation to Haskell

\TODO{Show how some language features get compiled very easily to
Haskell using quotation}
\TODO{Explain the use of Template Haskell and that it is optional.}

# Push arrays
\label{sec:push}

~~~
data Push sh a = 
  Push ((Shape sh -> a -> M ()) -> M ()) (Shape sh)
~~~

# Measurements

\TODO{Matrix Multiplication}
\TODO{Sobel filter}
\TODO{Blur}
\TODO{FFT}
\TODO{(Maybe scale up)}

# Discussion

\TODO{Future work: Implementing data structures vs algorithms}

# Related work

\TODO{Related work}