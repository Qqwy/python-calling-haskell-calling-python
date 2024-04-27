First, make sure the dynamic library is up-to-date.
Note that `Ouroboros.so` is a symlink in the main folder pointing to the output of `cabal build`.
If the output of your `cabal build` puts the output elsewhere (because of differing ghc/cabal versions etc.), you might have to update the symlink.

```bash
cabal build
```

Now, run the example:
```bash
python example.py
```
