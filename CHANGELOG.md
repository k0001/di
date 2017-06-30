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

