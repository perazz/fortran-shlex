# fortran-shlex
Modern Fortran port of Python's shlex shell-like lexer. This package implements the simple shlex lexer, inspired by the Python shlex module, and based on the Golang implementation. The interface comes with two 

## Install

Just copy and paste `shlex_module.f90` into your project. Alternatively, `fortran-shlex` can be used as a dependency in your [Fortran Package Manager]() project: 

```
[dependencies]
shlex = { git="https://github.com/perazz/fortran-shlex.git" }
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


## License

The source code in this repository is Licensed under MIT license (LICENSE-MIT or https://opensource.org/licenses/MIT).

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.

## See also

- [fortran-regex](https://github.com/perazz/fortran-regex)
- [Golang shlex](https://github.com/google/shlex)
- [Python shlex](https://docs.python.org/3/library/shlex.html)
- [Rust shlex](https://crates.io/crates/shlex)
