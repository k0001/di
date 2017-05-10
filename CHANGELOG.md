# Version HEAD

* Fix ISO8601 formatting of second fractions.

* Drop `mkDiTextStderr` and `mkDiTextFileHandle` in favour of `mkDiStringHandle`
  and `mkDiStringStderr`. The rationale is that we are already paying the costs
  of many `show` calls, and users of this library are quite likely to use
  `String`s anyway (since they, too, are likely using `show` results). We will
  bring back `Text` based `mkDiTextStderr` when we can make it performant.

* Rename the `path` and `msg` functions to `contrapath` and `contramsg`,
  flipping the order of their arguments so that the function comes first (like
  in `contramap`).

* Export `flush`.

* Swap the names beween the sync and async logging functions (e.g., `err'` vs
  `err`), encouraging the use of the async functions first.

* Stricter ordering of async messages.

* `push` now takes a list of `path`s


# Version 0.1

* Initial version.

