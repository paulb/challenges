(ns purely-functional.cipher)

;; Problem description
;;
;; - Treat all letters as uppercase, and convert them to uppercase if needed.
;; - The first alphabetical character of the string will not change.
;; - All subsequent alphabetical characters are shifted toward Z by the alphabetical position of the preceding alphabetical character.
;; - Non-alphabetical characters are left as-is.

;; Write an encoder and decoder for this cipher

;; Examples

;; (encode "") ;=> ""
;; (encode "a") ;=> "A"
;; (encode "hello") ;=> "HMQXA"
;; (encode "newsletter") ;=> "NSBPEQYNYW"
;; (encode "1 hug") ;=> "1 HCB"

;; (decode "") ;=> ""
;; (decode "1") ;=> "1"
;; (decode "HMQXA") ;=> "HELLO"

;; Note that you should always be able to decode a string that was encoded and get back the original string uppercased.

;; Personal modification: maintain case!

(def ^:private alphabet
  {:lowercase [\a \b \c \d \e \f \g \h \i \j \k \l \m
               \n \o \p \q \r \s \t \u \v \w \x \y \z]
   :uppercase [\A \B \C \D \E \F \G \H \I \J \K \L \M
               \N \O \P \Q \R \S \T \U \V \W \X \Y \Z]})

(def ^:private alpha-map
  {:lowercase (zipmap (:lowercase alphabet) (range 1 27))
   :uppercase (zipmap (:uppercase alphabet) (range 1 27))})

(def ^:private int-map
  {:lowercase (zipmap (range 1 27) (:lowercase alphabet))
   :uppercase (zipmap (range 1 27) (:uppercase alphabet))})

(defn- case-key
  [c]
  (if (= (clojure.string/lower-case c) (str c))
    :lowercase :uppercase))

(defn- positive-offset-char
  [c pos offset case-key*]
  (let [offset* (+ pos offset)
        offset* (if (> offset* 26) (- offset* 26) offset*)]
    (get-in int-map [case-key* offset*])))

(defn- negative-offset-char
  [c pos offset case-key*]
  (let [offset* (- pos offset)
        offset* (if (< offset* 0) (+ offset* 26) offset*)]
    (get-in int-map [case-key* offset*])))

(defn- cipher-char
  [c data f]
  (let [case-key* (case-key c)
        pos (get-in alpha-map [case-key* c])
        offset (:offset data)]
    [(if (and pos offset)
       (f c pos offset case-key*)
       c)
     pos]))

(defn- encoder
  [out c]
  (let [[coded-char offset] (cipher-char c out positive-offset-char)]
    {:encoded (conj (:encoded out) coded-char)
     :offset offset}))

(defn encode
  [plaintext]
  (->> (reduce encoder {:encoded [] :offset nil} plaintext)
       (:encoded)
       (apply str)))

(defn- decoder
  [out c]
  (let [[decoded-char _] (cipher-char c out negative-offset-char)]
    {:decoded (conj (:decoded out) decoded-char)
     :offset (get-in alpha-map [(case-key c) decoded-char])}))

(defn decode
  [encoded]
  (->> (reduce decoder {:decoded [] :offset nil} encoded)
       (:decoded)
       (apply str)))

;; Test cases.

(defn empty-encodes-empty
  []
  (assert (= (encode "") "")))

(defn char-encodes-char
  []
  (assert (= (encode "a") "a"))
  (assert (= (encode "Q") "Q")))

(defn all-lowercase-encodes-correctly
  []
  (assert (= (encode "hello") "hmqxa"))
  (assert (= (encode "newsletter") "nsbpeqynyw"))
  (assert (= (encode "1 hug") "1 hcb")))

(defn mixed-case-encodes-correctly
  []
  (assert (= (encode "Hello") "Hmqxa"))
  (assert (= (encode "neWsletTer") "nsBpeqyNyw"))
  (assert (= (encode "1 HuG") "1 HcB")))

(defn empty-decodes-empty
  []
  (assert (= (decode "") "")))

(defn char-decodes-char
  []
  (assert (= (decode "a") "a"))
  (assert (= (decode "Q") "Q")))

(defn all-lowercase-decodes-correctly
  []
  (assert (= (decode "hmqxa") "hello"))
  (assert (= (decode "nsbpeqynyw") "newsletter"))
  (assert (= (decode "1 hcb") "1 hug")))

(defn mixed-case-decodes-correctly
  []
  (assert (= (decode "Hmqxa") "Hello"))
  (assert (= (decode "nsBpeqyNyw") "neWsletTer"))
  (assert (= (decode "1 HcB") "1 HuG")))

(defn run-tests
  []
  (empty-encodes-empty)
  (char-encodes-char)
  (all-lowercase-encodes-correctly)
  (mixed-case-encodes-correctly)
  (empty-decodes-empty)
  (char-decodes-char)
  (all-lowercase-decodes-correctly)
  (mixed-case-decodes-correctly))
