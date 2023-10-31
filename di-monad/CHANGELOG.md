# Version 1.4.0

* The main change in this release is that `DiT` correctly logs exception at
  their throw site whether they are thrown through `MonadThrow` or through
  the underlying `m`.

  Supporting this feature required changing `DiT`'s internal representation.
  The public API changed to some extent to accomodate this. Some changes were
  necessary, and others weren't but we took the opportunity to make them since
  they improved the overall experience.

* Replaced `DiT` representation with an explicit sum type free monad with
  an explicit bind constructor.

* Introduced `MonadAtomically` as a superclass to `MonadDi`.

* Default `MonadAtomically` instance for `MonadTrans`formers
  over `MonadAtomically`.

* `MonadDi` was simplified. `ask` is the only required method.

* Default `MonadDi` instance for `MonadTrans`formers over `MonadDi`.

* Removed `diT`, `unDiT`, `runDiT` and `runDiT'`

* Introduced `run` and `run'` to run a `DiT` in a `IO`-based monad.

* Introduced `diatomically` to atomically run an `DiT` over `STM`.

* Removed `Seq path` from `onException`.

* Introduced `localT`, `dilift`.


# Version 1.3.6

* Add optional support for `CatchT`.


# Version 1.3.5

* Add optional support for `conduit` and `resourcet`.


# Version 1.3.4

* Fix typo in `unliftio-core` Cabal flag.


# Version 1.3.3

* Add `MonadBase`, `MonadBaseControl`, `PrimMonad` and `MonadSafe` instances
  for `DiT`.

* Add Cabal flags for optional third-party libraries support: `monad-control`,
  `pipes`, `pipes-safe`, `primitive`, `streaming`, `transformers-base`,
  `unliftio-core`.


# Version 1.3.2

* Add `MonadUnliftIO` instance for `DiT`.

* Add `MonadDi` instance for `streaming`'s `Stream`.


# Version 1.3.1

* Add `MonadError` instance for `DiT`.

# Version 1.3

* The `MonadThrow` instance for `DiT` doesn't log exceptions automatically any
  more. This is because otherwise catching and re-throwing exceptions in
  downstream code, by default, ends up logging the same exception more than
  once.

* A `throw` function behaving as the previous `MonadThrow` instance for `DiT`
  was introduced.

* Documentation improvements.


# Version 1.2

* The `MonadMask` constraint added in Version 1.1 is gone, effecively
  undoing the breaking change introduced in Version 1.1.


# Version 1.1

* **BREAKING CHANGE:** The `MonadThrow` instance for `DiT level path msg
  m` instance now relies on `Di.Core.throw`, potentially logging
  exceptions _at the throw site_.  This introduces a new `MonadMask m`
  instance constraint which can't be satisfied by `STM`. There is a
  `DiT level path msg STM` instance that skips logging exceptions (which
  is the only sensible behavior, anyway). However, if your `m` is not
  exactly `STM` but some wrapper around it, you will need to provide a
  `MonadThrow` instance manually or use `Di.Core.throw'` directly. On
  the other hand, satisfying this `MonadMask` constraint should be easy
  for monads that can run `IO`.

* Added `onException`.

# Version 1.0.2

* Backwards compatibility with `mtl < 2.2.2`.

# Version 1.0.1

* Backwards compatibility with `transformers < 0.5.3`.

# Version 1.0

* This is a new library part of the `di-core` ecosystem.
  Consider this first release of the new ecosystem a preview release: The API is
  likely to stay stable, but extensive testing, formalization and tooling is
  due.

