(ns purely-functional.core
  [:require [purely-functional.chess-moves :as chess-moves]]
  [:require [purely-functional.cipher :as cipher]])

(defn reload
  []
  (use 'purely-functional.chess-moves :reload-all)
  (use 'purely-functional.cipher :reload-all))

(defn -main
  [& args])
