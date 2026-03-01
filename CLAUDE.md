# Project Notes

## Overview
- This repository is NASIUM-LSE, a Common Lisp implementation of the L.S.E. (Langage Symbolique d'Enseignement) interpreter and system.
- Core sources live in `src/` and are loaded via ASDF systems.

## Key Systems
- `com.informatimago.lse` is the core interpreter/VM/compiler system.
- `com.informatimago.lse.cli` provides the Unix CLI.
- `com.informatimago.lse.server` provides the server (currently noted as non-functional in docs).

## Build/Run
- Dependencies are fetched with `dependencies/get-dependencies` (see `src/README`).
- `libfixposix` may need to be built and installed from `dependencies/libfixposix`.
- Development load path:
  - `(load "src/loader.lisp")` in a Lisp REPL (see `src/README`).
- CLI entrypoint:
  - `(com.informatimago.lse.cli:main)`
  - Or `make cli` in `src/` to build an executable.

## Features/Configuration
- Feature flags like `:lse-unix`, `:lse-case-insensitive`, `:lse-extensions` are set in `src/loader.lisp` and in generation scripts (`generate-*.lisp`).
- For runtime root paths, `LSE_ROOT` can be used; see `src/cli.lisp` and `src/README`.

## Tests/Diagnostics
- Parser/scanner tests exist as Lisp files in `src/` (e.g. `test-lse-parser.lisp`, `test-lse-scanner.lisp`). They are run manually from a Lisp REPL.

If you run a temp home, make it specific to you, and to the project, to avoid collision with parallel instances in other projects. eg. XDG_CACHE_HOME=/tmp/codex/nasium-lse

## Notes
- The repository includes example programs in `progr/` and `tapes-xoff/`.
- Documentation lives in `doc/` and is generated via `generate-documentation.lisp` / `make documentation`.
