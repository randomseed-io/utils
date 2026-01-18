# Random Utilities (by random:seed)

A small constellation of pragmatic Clojure utility libraries, published as **separate
artifacts** (modules) plus a **meta-package** that depends on all of them.

[![random:utils on cljdoc](https://cljdoc.org/badge/io.randomseed/utils)](https://cljdoc.org/d/io.randomseed/utils/CURRENT)
[![CircleCI](https://circleci.com/gh/randomseed-io/utils.svg?style=svg)](https://circleci.com/gh/randomseed-io/utils)

[![utils on Clojars](https://img.shields.io/clojars/v/io.randomseed/utils.svg)](https://clojars.org/io.randomseed/utils)
[![utils-core on Clojars](https://img.shields.io/clojars/v/io.randomseed/utils-core.svg)](https://clojars.org/io.randomseed/utils-core)
[![utils-bus on Clojars](https://img.shields.io/clojars/v/io.randomseed/utils-bus.svg)](https://clojars.org/io.randomseed/utils-bus)
[![utils-crypto on Clojars](https://img.shields.io/clojars/v/io.randomseed/utils-crypto.svg)](https://clojars.org/io.randomseed/utils-crypto)
[![utils-db on Clojars](https://img.shields.io/clojars/v/io.randomseed/utils-db.svg)](https://clojars.org/io.randomseed/utils-db)
[![utils-ip on Clojars](https://img.shields.io/clojars/v/io.randomseed/utils-ip.svg)](https://clojars.org/io.randomseed/utils-ip)
[![utils-log on Clojars](https://img.shields.io/clojars/v/io.randomseed/utils-log.svg)](https://clojars.org/io.randomseed/utils-log)
[![utils-reitit on Clojars](https://img.shields.io/clojars/v/io.randomseed/utils-reitit.svg)](https://clojars.org/io.randomseed/utils-reitit)
[![utils-time on Clojars](https://img.shields.io/clojars/v/io.randomseed/utils-time.svg)](https://clojars.org/io.randomseed/utils-time)
[![utils-validators on Clojars](https://img.shields.io/clojars/v/io.randomseed/utils-validators.svg)](https://clojars.org/io.randomseed/utils-validators)

## Install

Choose one:

### Meta-package (everything)

```clojure
io.randomseed/utils {:mvn/version "2.0.1"}
```

### A single module

```clojure
io.randomseed/utils-core {:mvn/version "2.0.1"}
```

## Modules

Each module is a standalone artifact (same versioning across the set):

- `utils` – meta-package: depends on all modules below
- `utils-core` – helpers for basic type conversions and checks
- `utils-bus` – asynchronous worker bus
- `utils-crypto` – cryptographic helpers
- `utils-db` – database abstractions with caching
- `utils-ip` – IP address support functions
- `utils-log` – opinionated logging facility
- `utils-reitit` – Reitit support abstractions
- `utils-time` – date and time operations
- `utils-validators` – data validation helpers

### Backward compatibility note

`utils-core` also ships a small set of functions directly in the root namespace
`io.randomseed.utils` (for compatibility with older code). This is intentionally
*not* implemented as delegation stubs.

## Namespaces

Current public namespaces (as published in the generated API docs):

- `io.randomseed.utils`,
- `io.randomseed.utils.bot`,
- `io.randomseed.utils.bus`,
- `io.randomseed.utils.crypto`,
- `io.randomseed.utils.crypto.codecs`,
- `io.randomseed.utils.db`,
- `io.randomseed.utils.db.types`,
- `io.randomseed.utils.fs`,
- `io.randomseed.utils.ip`,
- `io.randomseed.utils.log`,
- `io.randomseed.utils.log.logback`,
- `io.randomseed.utils.map`,
- `io.randomseed.utils.nop-cache`,
- `io.randomseed.utils.reitit.http`,
- `io.randomseed.utils.set`,
- `io.randomseed.utils.time`,
- `io.randomseed.utils.validators`,
- `io.randomseed.utils.validators.common`,
- `io.randomseed.utils.var`,
- `io.randomseed.utils.vec`.

## Development

This is a monorepo; modules live under `modules/<module>/` and are built/published as
independent artifacts.

Typical flow (names may vary depending on your local setup):

- run tests,
- build per-module JARs,
- publish per-module artifacts.

## License

Copyright © 2021-2025 Paweł Wilk

Random Utilities (utils) is copyrighted software owned by Paweł Wilk
(pw@gnu.org). You may redistribute and/or modify this software as long as you comply
with the terms of the [GNU Lesser General Public License][LICENSE] (version 3).

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

