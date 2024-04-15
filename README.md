# Not all that G[l]itters is Gold.

The idea is simple. Instead of predicting the output of a test, you simply run
the test, and save the results. Now, you can check if the test have changed,
according to the version control system.

# Features

Seperately from other packages, in Glitter test. 

1. There are no seperate tool for accepting and inspecting golden test. You can
   simply use git, as you would normally.

1. After running the golden test, you can run a sequence of checks to see if
   the saved file is well-formed. This way you don't save files without some
   minimal assurances.

# Missing Features

Eventually, I would like to add:

1. Templating. Normalizing output is hard and not always straight forward, when
   dates or other things are involved. In this case, it would be nice if the
   golden file, is a template that can simply check if it matches.

1. Force test mode. In a CI/CD system, you would like to test all cases, even
   the ones that havn't changed. Adding a flag to controll this would be nice.

1. Inline diffing.

# Related Packages

- [`hspec-golden`](https://hackage.haskell.org/package/hspec-golden)

- [`tasty-golden`](https://hackage.haskell.org/package/tasty-golden)
