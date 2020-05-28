# Version 0.4

* COMPILER ASSISTED BREAKING CHANGE: Change functions names `Df1.render` to
  `Df1.Render.log`, `Df1.renderColor` to `Df1.Render.logColorANSI`, and
  `Df1.parse` to `Df1.Parse.log`.

* Exported `key`, `message`, `iso8601`, `segment` and `value` from module
  `Df1.Render`.


# Version 0.3.2

* Added `ToValue` instances for common types like `Int`, `Bool`, etc.

* Changed colours in `Log` rendering.


# Version 0.3.1

* Added missing language pragma.


# Version 0.3

* The `segment`, `key`, `value` and `message` functions are now methods of the
  new `ToSegment`, `ToKey`, `ToValue` and `ToMessage` classes.  Identity
  instances, as well as instances for `String`, lazy `Text` and strict `Text`,
  have been introduced for these classes.


# Version 0.2

* BREAKING CHANGE: `Segment`, `Key`, `Value` and `Message` don't strip
  surrounding whitespace anymore. When rendering `Segment`, `Key` and `Value`,
  the whitespace will be percent-encoded. When rendering `Message`, the
  whitespace will be kept as is.

* BREAKING CHANGE: `Segment` and `Key` now wrap lazy `Text`, rather than strict
  `Text`. This is to align their APIs with `Value` and `Message`, which already
  wrapped lazy `Text` so as to prevent logged `Value`s and `Message`s from
  to use much memory. It's unlikely that `Segment`s and `Key`s are affected by
  this, since in practice they are almost always created statically. So, this
  change is mostly to make the API less surprising to users: Lazy `Text` is used
  throughout.

* Added draft BNF specification.


# Version 0.1.2

* Fixed escaping of control characters in `Message`.

* Percent-escape less punctuation characters when rendering `Key`,
  `Segment` and `Value`.

  TODO: write spec.


# Version 0.1.1

* Fixed compilation.


# Version 0.1

* Consider this a preview release: The API is likely to stay stable, but
  extensive testing, formalization and tooling is due.
