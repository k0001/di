# Version HEAD

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

