To compile:

runhaskell Setup.hs configure
runhaskell Setup.hs build

To run unit tests:

runhaskell Setup.hs test

To install:

runhaskell Setup.hs install

You can then run the resulting program as

  vintbas [<.bas source file> ...]

The monad transformer I created is CPST.  Monad
transformers are basically monad building-blocks.  You
start with the identity monad and stack monad
transformers on top of it to build a combined monad.
The ordering is very important.  There are monad
transformers in the Control.Monad.Trans library,
so I used them.  Unfortunately you can't stack any
more monad transformers on top of CPST.  It has to be on
top, because of the type of the shift morphism.  The
standard ContT transformer is similar to my CPST, but
defines callCC, not shift.  Take a look at the type
of callCC in my code.  Notice every time you see a
CPST, it is followed by an o.  Notice that every
other morphism in CPST has the same property, except
shift.  That is the key to why it's no longer stackable -
monad transformers can take only two type parameters, not
three.  But I like shift and think it's neat that I
can define callCC in terms of it and reset, but not
vice-versa.  So there.  Plus, my shift and callCC are
rank-3 polymorphic!  Nobody else achieves that flexibility.

Other things we could do:
* Pre-check types
* Pre-check labels, generate code in place of labels
* Convert variable references to IORefs
Is it easiest to do these with staging?
