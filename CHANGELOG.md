# Changelog

## [0.1.53] - unreleased

### Changed

- `swark.core/filter-keys` - Now always returns a map; previously could return `nil` in edge cases.

## [0.1.52] - released

### Added

- `swark.core/defmemo` - Macro that defines a memoized function and returns it.
  Supports `^:dynamic` on the name, making the var rebindable via `binding`.
  Accepts an optional memoizer as the second argument, defaulting to `memoize`.
  `(defmemo my-fn [x] (* x x))` uses `memoize`.
  `(defmemo my-fn memoir [x] (* x x))` uses `memoir`, gaining flush capability.

- `swark.core/flush-signal` - Sentinel value for triggering cache eviction in `memoir` fns.
  Replaces the previous `:flush` keyword convention, which prevented `:flush` from being
  used as a legitimate cached argument.
  `(f flush-signal)` evicts the entire cache. `(f flush-signal arg)` evicts a single entry.

### Fixed

- `swark.core/memoir` - Functions returning `nil` or `false` were not cached; the underlying function was called on every invocation. Fixed by replacing the `or`-based cache check with `contains?`.

- `swark.core/memoir` - The `:flush` keyword could never be used as a legitimate cached argument. Replaced the keyword sentinel with a private `Object.` instance (`flush-signal`) checked via `identical?`.
