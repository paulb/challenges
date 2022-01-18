(ns kroo.core
  (:gen-class)
  (:require [kroo.bowling :as bowling]))

(defn reload
  []
  (use 'kroo.bowling :reload-all))

(defn -main
  [& args])
