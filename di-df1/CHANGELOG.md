# Version 1.2

* COMPILER ASSISTED BREAKING CHANGE: Renaming logging functions again.
  For example, `info` is the name we give to the `ToMessage`-polymorphic
  logging functions, `info_` to their `Message`-monomorphic version, and
  `info'` to the `STM` version. The previous `STM` suffix in `infoSTM`
  is gone, so as to mimick the naming conventions of `di-core`.

* Export `attr_`.

# Version 1.1

* COMPILER ASSISTED BREAKING CHANGE: The `STM` compatible logging functions
  previously called `debug'`, `info'`, etc. are now called `debugSTM`,
  `infoSTM`, etc. The names such as `debug'`, `info'`, etc. are now what `debug`,
  `info`, etc. were in the previous version.

* Take `ToValue` and `ToMessage` instances where possible, rather than values of
  type `Value` and `Message`.

* `Di.Df1` now re-exports `Path`, `Segment`, `Key`, `Value`, `Message`,
  `ToKey(key)`, `ToValue(value)` and `ToMessage(message)`.


# Version 1.0.2

* Added `Df1`, `Df1T` and `MonadDf1` type-synonyms.


# Version 1.0.1

* Removed unnecessary `MonadIO` constraint.


# Version 1.0

* Consider this a preview release: The API is likely to stay stable, but
  extensive testing, formalization and tooling is due.

