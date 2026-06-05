# Changelog

## [0.1.52] - unreleased

### Added

- `swark.core/defmemo` - Macro that defines a memoized function and returns it.
  Supports `^:dynamic` on the name, making the var rebindable via `binding`.
  `(defmemo my-fn [x] (* x x))` expands to `(def my-fn (memoize (fn [x] (* x x))))`.

### Fixed

- `swark.core/memoir` - Functions returning `nil` or `false` were not cached; the underlying function was called on every invocation. Fixed by replacing the `or`-based cache check with `contains?`.
