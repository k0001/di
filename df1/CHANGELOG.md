# Version 0.2

* BREAKING CHANGE: `Key`, `Value` and `Message` don't strip surrounding
  whitespace anymore. When rendering `Key` and `Value`, the whitespace
  will be percent-encoded. When rendering `Message`, the whitespace will
  be kept as is.


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
