# Introduction

This is Random Utilities.

## Installation

To use Random Utilities in your project, add the following to dependencies section of
`project.clj` or `build.boot`:

```clojure
[io.randomseed/utils "1.0.1"]
```

For `deps.edn` add the following as an element of a map under `:deps` or
`:extra-deps` key:

```clojure
io.randomseed/utils {:mvn/version "1.0.1"}
```

You can also download JAR from
[Clojars](https://clojars.org/io.randomseed/utils).

Additionally you can use (in your development profile) if you want to utilize specs
and spec-integrated generators provided by the Random Utils:

```clojure
org.clojure/spec.alpha {:mvn/version "0.2.194"}
org.clojure/test.check {:mvn/version "1.1.0"}
```

## License

Copyright © 2021 Paweł Wilk

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

[LICENSE]:    https://github.com/randomseed-io/utils/blob/master/LICENSE

