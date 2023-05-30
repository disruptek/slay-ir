(use slay-ir/0.6)

(defn round-trip
  [value]
  (let [encoded (encode value)
        decoded (decode encoded)]
    (unless (deep= decoded value)
      (error (string/format "round-trip failed: %q -> %q -> %q"
                            value encoded decoded)))))

(round-trip 5)
(assert (deep= (encode nil) {:none 0}))
(assert (deep= (decode {:none 0}) :null))
(round-trip true)
(round-trip false)
(round-trip "str")
(round-trip ["a" "b" "c"])
(assert (deep= (encode @["a" nil]) @["a" {:none 0}]))
(assert (deep= (decode @["a" {:none 0}]) @["a" :null]))
(assert (deep= (encode ["a" nil]) @{:tuple @["a" {:none 0}]}))
(assert (deep= (decode @{:tuple @["a" {:none 0}]}) ["a" :null]))
(round-trip @[1 "a" true])
(round-trip @{1 "a"})
(assert (deep= (encode ["a" nil]) @{:tuple @["a" {:none 0}]}))
(assert (deep= (decode @{:tuple @["a" {:none 0}]}) ["a" :null]))
(assert (deep= (encode {"a" @[true nil]}) @{:dict @[["a" @[true {:none 0}]]]}))
(assert (deep= (decode {:dict [["b" 1] ["a" [true {:none 0}]]]}) @{"b" 1 "a" [true :null]}))
