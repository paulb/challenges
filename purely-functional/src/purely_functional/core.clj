(ns purely-functional.core
  [:require [purely-functional.cipher :as cipher]])

(defn reload
  []
  (use 'purely-functional.cipher :reload-all))

(defn -main
  [& args])
