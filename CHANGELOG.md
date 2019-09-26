## Unreleased

* Dropped support for GHC 8.2 and older.

## Forma 1.1.2

* Fixed the test suite so it passes with `aeson-1.4.3.0`.

## Forma 1.1.1

* Fixed a bug which caused `withCheck` (and functions using it such as
  `field`) report incorrect location of element for which validation fails
  when it's nested in `subParser` wrappers.

## Forma 1.1.0

* Added `runFormPure`.

## Forma 1.0.0

* The library has been completely redesigned and rewritten.

## Forma 0.2.0

* Added `withCheck`.

## Forma 0.1.0

* Initial release.
