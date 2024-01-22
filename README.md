# Swark

SWiss ARmy Knife - Your everyday clojure toolbelt!

https://clojars.org/org.clojars.stanv/swark

## Basic usage

### Clojure CLI/deps.edn

Add this dependency to the :deps map in deps.edn
`org.clojars.stanv/swark {:mvn/version "0.1.0"}`

Require swark.core in your ns form:
`(:require [swark.core :as swark])`

Then you can use the Swark utility functions:
`(swark/key-by :id [{:id 1 :name "one"} {:id 2 :name "two"}])`

## Tests

Run the tests with `clojure -X:tests`

## Development

Start a repl with `clojure -M:repl/reloaded`

Create an uberjar with `clj -X:uberjar :jar swark-x.x.x.jar` (replacing x's with version)

Install Swark locally with `clj -X:install`

## License

Swark by Stan Verberkt is marked with CC0 1.0 Universal 
