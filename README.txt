To compile:

ghc -fglasgow-exts --make Basic -o basic

You can then run the resulting program as

  basic [<.bas source file> ...]

When you run a program it will first show the original
source, then the prettyprinted source, then the output.

The included writeup.pdf is for an older version of
the program.  More things have been added since.  It
still lacks builtin functions, DEF FN, ON-GOTO/GOSUB,
and READ-DATA.  But it has a very nice INPUT!

I've improved the parser since then.  And the new version
also has a cleverer jumptable which effectively memoizes
interpreted code.  Sort of like a JIT!

Sorry, there aren't nearly enough comments in the code.

Note: You may wonder why I have an abstract Parser
interface, then a single instance called SimpleParser.
It seems like over-abstraction.  Well originally I just
had a Parser module and a BasicParser module that used
it.  Well the reason is that I was trying to develop
a new parser with lookahead, called LAParser.  I wrote
it but I had a serious typing problem with it, so I
aborted that idea.

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
