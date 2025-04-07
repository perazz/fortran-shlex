# fortran-shlex
Modern Fortran port of Python's shlex shell-like lexer. This package implements the simple shlex lexer, inspired by the Python shlex module, and based on the Golang implementation. The interface comes with two functions, `split` which parses a command-like string and returns an array of allocatable character strings; and `shlex` that perform the same, but return a list of `type(shlex_token)` tokens. Error control is optional, via a boolean `success` keyword or a token that may return an error string.

## Install

Just copy and paste `shlex_module.f90` into your project. Alternatively, `fortran-shlex` can be used as a dependency in your [Fortran Package Manager]() project: 

```
[dependencies]
fortran-shlex = { git="https://github.com/perazz/fortran-shlex.git" }
```
  
## Usage

The `split` function returns a list of strings, split according to unix shell rules, which support: 
- escaping quotes (`"..."`)
- non-escaping quotes (`'...'`)
- line feed, carriage return etc.

```fortran
use shlex_module

character(len=:), allocatable :: tokens(:)
type(shlex_token) :: error
logical :: success

! Simple usage
tokens = split('my -W"ery" -Llong //Input \n "string"')

! With logical error flag
tokens = split('whatever ',success=success)

! With complete error flag
tokens = split('whatever ',error)
print *, 'error message=',error%string
```

And the `shlex` function has the same API, but returns a list of `type(shlex_token)`s instead of an allocatable character array. 

```fortran
use shlex_module

type(shlex_token), allocatable :: tokens(:)
type(shlex_token) :: error
logical :: success

! Simple usage
tokens = shlex('my -W"ery" -Llong //Input \n "string"')

! With logical error flag
tokens = shlex('whatever ',success=success)

! With complete error flag
tokens = shlex('whatever ',error)
print *, 'error message=',error%string
```

---

## Version 1.1.0 - `join_spaced` to combine spaced compiler flags

Starting with version **1.1.0**, a new high-level interface is available:

```fortran
tokens = split('gfortran -I /include -L /lib -lm', join_spaced=.true., success=success)
```

When the second argument (`join_spaced`) is `.true.`, `split_joined_bool()` will:
- combine spaced flags like `-I /path` into `-I/path`
- work for any single-letter flags (e.g., `-I`, `-L`, `-D`) if the next token does *not* begin with `-`
- still respect quotes and shell-like rules for escaping

This is useful for parsing compiler and linker flags where `-I`, `-L`, etc. may be followed by a separate token due to quoting or formatting.

---

## License

The source code in this repository is Licensed under MIT license (LICENSE-MIT or https://opensource.org/licenses/MIT).

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.

## See also

- [fortran-regex](https://github.com/perazz/fortran-regex)
- [Golang shlex](https://github.com/google/shlex)
- [Python shlex](https://docs.python.org/3/library/shlex.html)
- [Rust shlex](https://crates.io/crates/shlex)
