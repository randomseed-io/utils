# Introduction

**Random Utilities – a bunch of support functions.**

[![random:utils on Clojars](https://img.shields.io/clojars/v/io.randomseed/utils.svg)](https://clojars.org/io.randomseed/utils)
[![random:utils on cljdoc](https://cljdoc.org/badge/io.randomseed/utils)](https://cljdoc.org/d/io.randomseed/utils/CURRENT)
[![CircleCI](https://circleci.com/gh/randomseed-io/utils.svg?style=svg)](https://circleci.com/gh/randomseed-io/utils)

Clojure library with helpful functions and macros.

## Features

TBW

## Installation

To use random:utils in your project, add the following to dependencies section of
`project.clj` or `build.boot`:

```clojure
[io.randomseed/utils "1.2.26"]
```

For `deps.edn` add the following as an element of a map under `:deps` or
`:extra-deps` key:

```clojure
io.randomseed/utils {:mvn/version "1.2.26"}
```

Additionally, if you want to utilize specs and generators provided by the random:utils
you can use (in your development profile):

```clojure
org.clojure/spec.alpha {:mvn/version "0.2.194"}
org.clojure/test.check {:mvn/version "1.1.0"}
```

You can also download JAR from [Clojars](https://clojars.org/io.randomseed/utils).

## Sneak peeks

TBW

And more…

## Documentation

Full documentation including usage examples is available at:

* https://randomseed.io/software/utils/

## License

Copyright © 2021-2022 Paweł Wilk

random:utils is copyrighted software owned by Paweł Wilk (pw@gnu.org). You may
redistribute and/or modify this software as long as you comply with the terms of
the [GNU Lesser General Public License][LICENSE] (version 3).

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

## Development

### Building docs

```bash
make docs
```

### Building JAR

```bash
make jar
```

### Rebuilding POM

```bash
make pom
```

### Signing POM

```bash
make sig
```

### Deploying to Clojars

```bash
make deploy
```

### Interactive development

```bash
bin/repl
```

Starts REPL and nREPL server (port number is stored in `.nrepl-port`).

[LICENSE]:    https://github.com/randomseed-io/utils/blob/master/LICENSE
