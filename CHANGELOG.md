## Forma 1.0.0

* Changed signature of `mkFieldError` to accept `NonEmpty (SelectedName names)` 
  as first argument.
* Changed `runForm` function to return `BranchState`.
* Added `toResponse` that converts `BranchState` to `Response`, which can be 
  serialized to JSON.
* Added `subParser` and `value` combinators.
* Exported `showFieldPath` and `FieldError`'s value constructor.

## Forma 0.2.0

* Added `withCheck`.

## Forma 0.1.0

* Initial release.
