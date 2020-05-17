# Version 1.0.5

* Fix test that failed when clock resolution is too small. See #44.


# Version 1.0.4

* Improve exception handling when worker stops.

* Added `LoggingWorkerNotRunning`.

* Fixed path rendering when logging exceptions.


# Version 1.0.3

* Fix handling of async exceptions even more (now with tests).

* The `exceptions` dependency is gone.

* Added `ExceptionInLoggingWorker`.

* Documentation improvements.


# Version 1.0.2

* The `MonadMask` superclass of `throw'` is gone.

* Fix handling of async exceptions.

* Add dependency on `safe-exceptions`.


# Version 1.0.1

* Added `throw'`, `throw` and `onException`.


# Version 1.0

* This library contains most of what was in `di-0.3`. Consider this first
  release of the new ecosystem a preview release: The API is likely to stay
  stable, but extensive testing, formalization and tooling is due.

