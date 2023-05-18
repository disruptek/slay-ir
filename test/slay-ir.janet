(import spork/json)

(import slay-ir/0.6 :as slay-ir)
(import slay-ir/uri-encoding :as url)
(use slay-ir)

(def data @{"one" 1 "two" true "three" @[1 2 3]})
(def encoded (encode-json data))
(def decoded (decode-json encoded))
(assert (deep= decoded data))

(assert (= :null (slay-ir/decode {:ptr "json:null"})))
(assert (= true (slay-ir/decode {:ptr "json:true"})))
(assert (= 5 (slay-ir/decode {:ptr "json:5"})))

(let [js (json/encode @["👍" {:a 1}])
      encoded (url/encode js)
      decoded (slay-ir/decode {:ptr (string "json:" encoded)})]
  (assert (deep= decoded @["👍" @{:a 1}])))
