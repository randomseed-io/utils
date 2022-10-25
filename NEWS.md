# History of random:utils releases

## 1.2.19 (2022-10-25)

- Multiple database accessing funtions optimized for speed.

## 1.2.18 (2022-09-18)

- `map/update-missing` now takes only unary functions by default,
  without shifting the arguments.

## 1.2.17 (2022-09-09)

- `map/assoc-if` and `map/assoc-if-not` can now handle multiple pairs.

## 1.2.16 (2022-09-01)

- Added `some-str-down`.

- Performance improvements in:
  `empty-string?`, `not-empty-string?`,
  `some-str`, `some-str-up`, `some-str-simple`,
  `some-str-simple-up`, `some-str-simple-down`,
  `some-string`.

## 1.2.15 (2022-08-30)

- Optimizations in `ip/bytes-to-ipv4` and `ip/bytes-to-ipv6`.
- Bug fixed in `db/get-cached-prop` (`list` -> `list*`).

## 1.2.14 (2022-08-28)

- Added `str-squeeze-spc`, `some-str-squeeze-spc`.

## 1.2.13 (2022-08-25)

- IP predicates improved, safer unsigned bytes support.

## 1.2.12 (2022-08-25)

- Multiple improvements in IP address conversion functions.

## 1.2.11 (2022-08-09)

- Added `db.types/add-setter-phone-number`.

## 1.2.10 (2022-08-08)

- Added multary variant of settings getter produced by `db/make-setting-getter`.
- Added multary variant of `db/cached-setting-get`.
- Keying improved in cached settings (was vector-based, now it's keyword-based).

## 1.2.9 (2022-07-28)

- Improved `validators/explain` and `validators/validate`.
- Introduced 3 operational modes of required parameters validation:
  - matching all of the required parameters,
  - matching at least 1 required parameter,
  - matching at least n required parameters.
- Replaced `when` with `if` where appropriate.

## 1.2.8 (2022-07-25)

- Added `validators/explain`.

## 1.2.7 (2022-07-17)

- Added `map/map-values-with-path` and `map/map-values-with-rpath`.

## 1.2.6 (2022-07-15)

- Common validators improved.

## 1.2.5 (2022-05-26)

- Memoization improved.

## 1.2.4 (2022-05-15)

- Added `var/update`.

## 1.2.3 (2022-04-27)

- Lazy map support functions added compatible with `com.intuitiveexplanations/lazy-map`:
  `map/lazy`, `map/to-lazy`, `map/lazy?`, `map/select-keys-lazy`, `map/merge-lazy`.

## 1.2.2 (2022-04-22)

- Added `db.types/add-setter-uuid`.

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

- Added `some-symbol`, `some-symbol-up`, `some-symbol-simple`, `simple-symbol-up`.

## 1.1.5 (2022-04-14)

- Fixed a bug in `map/nil-keys` and `map/nil-existing-keys` causing error when there was no matching key.

## 1.1.4 (2022-04-03)

- Added console reading functions: read-line-with-prompt, crypto/read-pwd, crypto/read-key.
- Added high-level text getting functions: ask, crypto/ask-pass, crypto/ask-key.

## 1.1.3 (2022-04-02)

- Database setter generator simplified.

## 1.1.2 (2022-04-01)

- Removed Clojure 11.1.0 dependency.
- Added backward compatibility with Clojure versions < 11.1.0.

## 1.1.1 (2022-04-01)

- Dependencies adjusted for Clojure 1.11.0.
- Updated CircleCI image reference.

## 1.1.0 (2022-04-01)

- Renamed parse-long to some-long.
- Removed random-uuid.
- Renamed uuid to to-uuid.

## 1.0.7 (2022-04-01)

- Fixed a bug causing empty keywords not being detected by empty-ident?.
- Added not-empty-string? and not-empty-ident?.
- Improved valuable? and not-valuable?.
- Added macros: not-valuable, when-not-valuable.

## 1.0.6 (2022-03-30)

- Added empty-string? and empty-ident?.
- Improved valence testing.

## 1.0.5 (2022-03-29)

- Added Reitit support functions.

## 1.0.4 (2022-03-28)

- Improved valence testing in valuable? and not-valuable?.

## 1.0.3 (2022-03-24)

- Fixed typo causing db/make-getter to produce unusable ternary function.

## 1.0.2 (2022-01-02)

- Better handling of sequential collections in time/parse-duration.

## 1.0.1 (2021-12-30)

- Improvements in time/parse-duration (multiple arguments support).

## 1.0.0 (2021-12-28)

- Initial release.
