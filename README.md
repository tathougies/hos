Hos: A Haskell operating system
--------------------------------

The kernel is totally in Haskell... JHC actually.

There are some bugs with monad transformers in JHC which make the compiler crash. You may see a funny idiom where we bind monadic calls to variables x1, x2, etc. and then `seq` them together before returning. This prevents an over-eager optimization from running which messes up the compile. At some point, I will spend time to patch JHC, but until then, the code compiles and seems to run.