# Version 1.4.0

* Removed `runDiT`.

* Re-export `run`, `diatomically`, `log`, `flush`, and `localT` from
  `Di.Monad`.


# Version 1.3

* COMPILER ASSISTED BREAKING CHANGE: Renaming logging functions again.
  For example, `info` is the name we give to the `ToMessage`-polymorphic
  logging functions, `info_` to their `Message`-monomorphic version, and
  `info'` to the `STM` version. The previous `STM` suffix in `infoSTM`
  is gone, so as to mimick the naming conventions of `di-core`.

* Re-export `attr_` from `Di.Df1.Monad`.

* Re-export all of `MonadDi(..)` from `Di.Df1.Monad`.

# Version 1.2.2

* Re-export `ask` from `Di.Df1.Monad`.


# Version 1.2.1

* Improve logging of exceptions.

* Documentation improvements.

* Re-export `debug'`, `info'`, etc. from `Di.Df1.Monad`.


# Version 1.2

* The `MonadThrow` instance for `DiT` doesn't log exceptions automatically any
  more. This is because otherwise catching and re-throwing exceptions in
  downstream code, by default, ends up logging the same exception more than
  once.

* A `throw` function behaving as the previous `MonadThrow` instance was
  introduced.


# Version 1.1.1

* Documentation improvements.

* Re-export `Df1.ToSegment`, `Df1.segment`, `Df1.ToMessage`, `Df1.message`,
  `Df1.ToKey`, `Df1.key`, `Df1.ToValue`, `Df1.value`.

# Version 1.1

* **BREAKING CHANGE:** Exceptions are now logged _at the throw site_ by
  default now when possible, with level `Warning`. See the changelog for
  `di-monad-1.1`.


# Version 1.0.1

* COMPILER ASSISTED BREAKING CHANGE: We don't export `Di`, `DiT` nor `MonadDi`
  anymore.

* Re-export `Df1.Path`, `Df1.Level`, `Di.Df1.Df1`, `Di.Df1.Monad.Df1T`,
  `Di.Df1.Monad.MonadDf1`.



# Version 1.0

* BREAKING CHANGE: Most of what used to be in this library lives now in
  `di-core`. This library is now intended to be an entry point to the various
  `di-*` libraries. Consider this first release of the new ecosystem a preview
  release: The API is likely to stay stable, but extensive testing,
  formalization and tooling is due.

# Version 0.3

* BREAKING CHANGE: `mkDiTextStderr` and `mkDiStringHandle` return a `Di String
  [String] String` now.

* Made compatible with GHC 8.4.


# Version 0.2

* BREAKING CHANGE: `Di` now takes a new type argument `level`.

* BREAKING CHANGE: Remove `Level` and all related functions in favour of a
  new `level` argument to `Di` to be implemented by the user.

* BREAKING CHANGE: Require `Monoid` instance for the `path` type.

* BREAKING CHANGE: Removed `level` function. Added `filter` function instead.

* BREAKING CHANGE: Drop `mkDiTextStderr` and `mkDiTextFileHandle` in favour of
  `mkDiStringHandle` and `mkDiStringStderr`. The rationale is that we are
  already paying the costs of many `show` calls, and users of this library are
  quite likely to use `String`s anyway (since they, too, are likely using `show`
  results). We will bring back `Text` based `mkDiTextStderr` when we can make it
  performant.

* BREAKING CHANGE: Rename the `path` and `msg` functions to `contrapath` and
  `contramsg`, flipping the order of their arguments so that the function comes
  first (like in `contramap`).

* BREAKING CHANGE: The `push` function now takes the `Di` value as second
  argument.

* Fix ISO8601 formatting of second fractions.

* Export `flush`.

* Stricter ordering of async messages.

* Added tests.

* Added a lot of documentation.


# Version 0.1

* Initial version.

