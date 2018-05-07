# Forma

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/forma.svg?style=flat)](https://hackage.haskell.org/package/forma)
[![Stackage Nightly](http://stackage.org/package/forma/badge/nightly)](http://stackage.org/nightly/package/forma)
[![Stackage LTS](http://stackage.org/package/forma/badge/lts)](http://stackage.org/lts/package/forma)
[![Build Status](https://travis-ci.org/mrkkrp/forma.svg?branch=master)](https://travis-ci.org/mrkkrp/forma)

This module provides a tool for validation of forms that are represented in
the JSON format. Sending forms in JSON format via an AJAX request instead of
traditional submitting of forms has a number of advantages:

* Smoother user experience: no need to reload the whole page.

* Form rendering is separated and lives only in GET handler, POST (or
  whatever method you deem appropriate for your use case) handler only
  handles validation and actual effects that form submission should
  initiate.

* You get a chance to organize form input just like you want.

The task of validation of a form in the JSON format may seem simple, but
it's not trivial to get it right. The library allows you to:

* Define form parser using type-safe applicative notation with field labels
  being stored on the type label which excludes any possibility of typos and
  will force all your field labels be always up to date.

* Parse JSON `Value` according to the definition of form you created.

* Stop parsing immediately if given form is malformed and cannot be
  processed.

* Validate forms using any number of *composable* checkers that you write
  for your specific problem domain. Once you have a vocabulary of checkers,
  creation of new forms is just a matter of combining them, and yes they do
  combine nicely.

* Collect validation errors from multiple branches of parsing (one branch
  per form field) in parallel, so validation errors in one branch do not
  prevent us from collecting validation errors from other branches. This
  allows for a better user experience as the user can see all validation
  errors at the same time.

* Use `optional` and `(<|>)` from `Control.Applicative` in your form
  definitions instead of ugly ad-hoc stuff (yes `digestive-functors`, I'm
  looking at you).

* When individual validation of fields is done, you get a chance to perform
  some actions and either decide that form submission has succeeded, or
  indeed perform additional checks that may depend on several form fields at
  once and signal a validation error assigned to a specific field(s). This
  constitute the “second level” of validation, so to speak.

## Example of use

* [examples/Main.hs](examples/Main.hs)

You may want to play with it a bit before writing serious code.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/forma/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2017–2018 Mark Karpov

Distributed under BSD 3 clause license.
