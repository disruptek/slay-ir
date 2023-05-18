(use slay-ir/0.4)

(defn round-trip
  [value]
  (let [encoded (encode value)
        decoded (decode encoded)]
    (unless (deep= decoded value)
      (error (string/format "round-trip failed: %q -> %q -> %q"
                            value encoded decoded)))))

(round-trip 5)
(assert (deep= (encode nil) "none:"))
(assert (deep= (decode "none:") :null))
(round-trip true)
(round-trip false)
(round-trip "str")
(round-trip ["a" "b" "c"])
(assert (deep= (encode @["a" nil]) @{:list @["str:a" "none:"]}))
(assert (deep= (decode @["str:a" "none:"]) @["a" :null]))
(assert (deep= (encode ["a" nil]) @{:tuple @["str:a" "none:"]}))
(assert (deep= (decode @{:tuple @["str:a" "none:"]}) ["a" :null]))
(round-trip @[1 "a" true])
(round-trip @{1 "a"})
(assert (deep= (encode ["a" nil]) @{:tuple @["str:a" "none:"]}))
(assert (deep= (decode @{:tuple @["str:a" "none:"]}) ["a" :null]))
(assert (deep= (encode {"a" @[true nil]}) @{:dict @[["str:a" @{:list @["bool:True" "none:"]}]]}))
(assert (deep= (decode {:dict [["str:a" ["bool:True" "none:"]]]}) @{"a" [true :null]}))
