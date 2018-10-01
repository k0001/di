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

