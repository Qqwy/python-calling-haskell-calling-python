```bash
# First, make sure the dynamic library is up-to-date.
# Note that there is a symlink in the main folder; if the output of your `cabal build` puts the output elsewhere (because of differing ghc/cabal versions), you might have to update the symlink.
cabal build

# Now, run the example:
python example.py
```
