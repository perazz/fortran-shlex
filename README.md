# fortran-shlex

**Modern Fortran port of Python's `shlex` shell-like lexer, now with Windows `mslex` support.**

This package provides two main lexer interfaces:
- `shlex`: a Unix-style shell lexer, modeled after Python's `shlex`, and
- `mslex`: a Windows-style lexer that emulates `cmd.exe` and `CommandLineToArgvW` behavior, inspired by the `mslex` Python module.

## 🆕 Version 2.0.0 — MSLEX for Windows/DOS-style argument splitting

The new **MSLEX API** allows parsing and quoting of strings according to Windows command-line conventions, which differ from POSIX shells.

#### `tokens = ms_split(pattern, like_cmd, ucrt, success)`
#### `tokens = ms_split(pattern, like_cmd, ucrt, error)`

Split a command-line string into arguments using Windows semantics.

**Arguments:**
- `pattern` (required): a `character(*)` input string.
- `like_cmd` (optional): if `.true.` (default), emulate both `cmd.exe` and `CommandLineToArgvW`.
- `ucrt` (optional): if `.true.`, parse using UCRT (modern CRT); use `msvcrt.dll` if `.false.`. If not provided, both methods are compared and if they differ (ambiguous pattern), an error is raised.
- `success` (optional): returns `.false.` if the string is invalid.
- `error` (optional): alternative error return method, returns a `type(shlex_token)` containing an error flag and message.

```fortran
character(len=:), allocatable :: args(:)
logical :: ok

args = ms_split('"my file.txt" -DVALUE=42', like_cmd=.true., success=ok)
if (.not.ok) print *, 'mslex error'
```

#### `quoted = ms_quote(s, for_cmd)`

Quote a string so that it is parsed correctly by Windows command-line interpreters.

- If `for_cmd = .true.`, quotes for `cmd.exe`, then `CommandLineToArgvW`.
- If `for_cmd = .false.`, quotes for direct use with `CommandLineToArgvW`.

```fortran
print *, ms_quote('my file.txt')                 ! → "my file.txt"
print *, ms_quote('^& dangerous', for_cmd=.true.) ! → "^& dangerous"
```

---

## 🔧 Install

Just copy and paste `shlex_module.f90` into your project. Or, use as a dependency in your [Fortran Package Manager](https://github.com/fortran-lang/fpm) project:

```toml
[dependencies]
fortran-shlex = { git="https://github.com/perazz/fortran-shlex.git" }
```

---

## 🐚 POSIX-like Usage (default `split` / `shlex`)

#### `tokens = split(string, [success], [error], [join_spaced], [keep_quotes])`

Splits a command-like string using Unix shell rules.

```fortran
character(len=:), allocatable :: tokens(:)
tokens = split('gfortran -I /include "quoted string"')
```

#### `tokens = shlex(string, [success], [error], [join_spaced], [keep_quotes])`

Same as `split`, but returns an `type(shlex_token), allocatable, dimension(:)` array.

---

## 📦 Feature Overview

### ✅ Basic Features
- Handles escaping quotes (`"..."`) and non-escaping quotes (`'...'`)
- Skips newline and carriage return characters
- Returns an array of tokens or structured tokens

---

## 🔧 Version 1.1.0 — `join_spaced` flag

Use `join_spaced=.true.` to automatically combine flags and paths like:

```fortran
tokens = split('gfortran -I /include -L /lib', join_spaced=.true.)
! Result: ["gfortran", "-I/include", "-L/lib"]
```

Works for any single-letter compiler flags (`-I`, `-L`, `-D`, etc.) where the path does not start with `-`.

---

## 🔍 Version 1.2.0 — `keep_quotes` flag

Use `keep_quotes=.true.` to retain original quoting:

```fortran
tokens = split('"quoted string" unquoted', keep_quotes=.true.)
! Result: ['"quoted string"', 'unquoted']
```

Note: if using `join_spaced`, you must also pass `keep_quotes` (both arguments required when either is set).

---

## 📜 License

This project is licensed under the MIT License:  
See `LICENSE-MIT` or <https://opensource.org/licenses/MIT>.

By contributing, you agree that your contributions may be dual licensed under the MIT and Apache-2.0 licenses.

---

## 🔗 See also

- [fortran-regex](https://github.com/perazz/fortran-regex)
- [Python shlex](https://docs.python.org/3/library/shlex.html)
- [Python mslex](https://github.com/smoofra/mslex)
- [Golang shlex](https://github.com/google/shlex)
- [Rust shlex](https://crates.io/crates/shlex)
