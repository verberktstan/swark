# Swark

SWiss ARmy Knife - Your everyday clojure toolbelt!

[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.stanv/swark.svg)](https://clojars.org/org.clojars.stanv/swark)

This library contains functionality that you might need every single day as a happy clojure(script) developer.
The aim is to provide composable functions that you can use everyday, but are just not as trivial as `(some (comp #{42} :answer) answers)`.

All functionality *should* work in both Clojure and Clojurescript.

## Basic usage

### Clojure CLI/deps.edn

Add this dependency to the :deps map in deps.edn:

```org.clojars.stanv/swark {:mvn/version "0.1.4"}```

Require swark.core in your ns form:

```(:require [swark.core :as swark])```

Then you can use the Swark utility functions:

```(swark/key-by :id [{:id 1} {:id 2}]) => {1 {:id 1} 2 {:id 2}}```

```(swark/map-vals count {:a [:b :c] :d [:e]}) => {:a 2 :d 1}```

```(swark/jab / 10 0) => nil```

## Little tour of Swark utilities

- `key-by`: Returns a map where all items are keyed by the result of calling (f item)
- `map-vals`: Returns a map where f is applied to all items in the input map.
- `filter-keys`: Returns a map containing only the map-entries whose key returns logical true when supplied to a predicate fn.
- `select-namespaced`: Returns a map containing only those map-entries whose key's namespace is equal to the supplied namespace.
- `jab`: Try and fail silently, returning nil when any kind of error or exception is thrown.
- `->str`: Returns input coerced to a (trimmed) string. Support (namesapced) keywords etc.
- `unid`: Return a unique id string.
- `->keyword`: Returns input coerced to a keyword, replacing whitespace with dashes.
- `invalid-map?`: Minimalistic spec checker, returns logical true if the input does not respect the spec-map. Spec map is simply a map with predicates as vals.
- `valid-map?`: Complement of invalid-map?
- `memoir`: Like memoize, but with flushing. Flush the complete cache, or specific parts.

## Example - Integrate swark.authom & swark.cedric

> Note: Cedric's CSV implementation is currently clj only! In cljs you can actually use the in-memory implementation (see swark.cedric/Mem)

Let's say you want to store a user record, some credentials and check their credentials.
You can use swark.cedric for the persistence part, and swark.authom for the authentication part.

1. Let's create/connect to a database via the Csv implementation and store db props related to users.

 ```
(ns my.ns
    (:require [swark.authom :as authom]
              [swark.cedric :as cedric])
    (:import [swark.cedric Csv]))

(def DB (cedric/Csv. "db.csv"))
(def PROPS (merge authom/CEDRIC-PROPS {:primary-key :user/id}))
 ```

2. Create a new user record with `cedric/upsert-items`:

```
(def USER (-> DB (cedric/upsert-items PROPS [{:user/name "Readme User"}]) first))
```

3. Store credentials (generated with `authom/with-token`) by upserting the user with `cedric/upsert-items`

```
(let [user (authom/with-token USER :user/id "pass" "SECRET")]
    (cedric/upsert-items DB PROPS [user]))
```

4. Retrieve the user with `cedric/find-by-primary-key` and check their credentials with `authom/check`:

```
(let [user (-> DB (cedric/find-by-primary-key #{:user/id} {:where (comp #{"Readme User"} :user/name)}) first)]
    (-> user (authom/check :user/id "pass" "SECRET") assert))
```

## Tests

Run clojure tests with `clojure -X:test/run`

Run cljs tests with `clojure -X:test-cljs/run -M unit-cljs`

## Development

Start a repl simply by running `clj -M:repl/basic` command in your terminal.
You can connect your editor via nrepl afterwards, e.g. from emacs; `cider-connect-clj`
Or create a repl from your editor, e.g. from emacs; `cider-jack-in-clj`

### Jar creation

Create an uberjar with `clj -X:uberjar :jar swark-0.1.4.jar`

### Local installation

Install Swark locally with `clj -X:install`

## License

Swark by Stan Verberkt is marked with CC0 1.0 Universal 
