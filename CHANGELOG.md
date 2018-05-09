## Forma 1.0.0

* Added `subParser` and `value` combinators. Changed signature of
  `mkFieldError` to accept `NonEmpty (SelectedName names)` as first
  argument.

* Improved typing, thus changing signature of `runForm` and introducing an
  additional type variable to the `FieldError` data type to reflect type of
  validation errors.

* Added `unFieldError`.

## Forma 0.2.0

* Added `withCheck`.

## Forma 0.1.0

* Initial release.
