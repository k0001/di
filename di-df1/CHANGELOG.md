# Version HEAD

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

