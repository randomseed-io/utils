# History of random:utils releases

## 1.2.1 (2022-04-21)

- Removed type hints for primitive locals.

## 1.2.0 (2022-04-20)

- Numeric ID keys support in generic database getters and setters
- Database builders and converters greatly simplified (removed some of the predefined options)
- More verbose error reporting during de-serialization
- Added `io.randomseed.utils.db.types` with opinionated database readers and writers
- Improved `some-str-*` functions, added filtering out empty strings in `some-str-spc`
- Added string processing functions:
  `to-lisp-str`, `to-snake-str`, `to-lisp-simple-str`, `to-snake-simple-str`
  `replace-first`, `to-lisp-str-replace-first`, `to-snake-str-replace-first`,
  `to-lisp-slashed-str`, `to-snake-slashed-str`.


## 1.1.6 (2022-04-16)

- Added some-symbol, some-symbol-up, some-symbol-simple, simple-symbol-up

## 1.1.5 (2022-04-14)

- Fixed a bug in map/nil-keys and map/nil-existing-keys causing error when there was no matching key

## 1.1.4 (2022-04-03)

- Added console reading functions: read-line-with-prompt, crypto/read-pwd, crypto/read-key
- Added high-level text getting functions: ask, crypto/ask-pass, crypto/ask-key

## 1.1.3 (2022-04-02)

- Database setter generator simplified

## 1.1.2 (2022-04-01)

- Removed Clojure 11.1.0 dependency
- Added backward compatibility with Clojure versions < 11.1.0

## 1.1.1 (2022-04-01)

- Dependencies adjusted for Clojure 1.11.0
- Updated CircleCI image reference

## 1.1.0 (2022-04-01)

- Renamed parse-long to some-long
- Removed random-uuid
- Renamed uuid to to-uuid

## 1.0.7 (2022-04-01)

- Fixed a bug causing empty keywords not being detected by empty-ident?
- Added not-empty-string? and not-empty-ident?
- Improved valuable? and not-valuable?
- Added macros: not-valuable, when-not-valuable

## 1.0.6 (2022-03-30)

- Added empty-string? and empty-ident?
- Improved valence testing

## 1.0.5 (2022-03-29)

- Added Reitit support functions

## 1.0.4 (2022-03-28)

- Improved valence testing in valuable? and not-valuable?

## 1.0.3 (2022-03-24)

- Fixed typo causing db/make-getter to produce unusable ternary function

## 1.0.2 (2022-01-02)

- Better handling of sequential collections in time/parse-duration.

## 1.0.1 (2021-12-30)

- Improvements in time/parse-duration (multiple arguments support).

## 1.0.0 (2021-12-28)

- Initial release.


