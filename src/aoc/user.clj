(ns aoc.user
  (:require
   [clojure+.error :as c+error]
   [clojure+.hashp :as c+hashp]
   [clojure+.print :as c+print]))

(defn init []
  (println "Setting things up for REPL development...")
  (println "Installing clojure+/hashp...")
  (c+hashp/install!)
  (println "Installing clojure+/print...")
  (c+print/install!)
  (println "Installing clojure+/error...")
  (c+error/install!)
  (println "Setup done."))

