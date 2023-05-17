(use spork/test)

(import slay-ir/0.6 :as slay-ir)
(use slay-ir)

(def data @{"one" 1 "two" true "three" @[1 2 3]})
(def encoded (encode-json data))
(def decoded (decode-json encoded))
(assert (deep= decoded data))
