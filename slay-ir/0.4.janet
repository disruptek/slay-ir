#
# slay/ir 0.4
#

(varfn encode
  `create something we can turn into valid json slay/ir`
  [value]
  (match (type value)
    :keyword
      (string "str:" value)   # we'll perform :keyword -> "keyword" ourselves
    :number
      (if (string/find "." (string value))
        (string "float:" value)
        (string "int:" value))
    :string
      (string "str:" value)
    :boolean
      (if value "bool:True" "bool:False")
    :nil
      "none:"                 # we'll perform the nil->:null ourselves
    :array
      @{:list (map encode value)}
    :tuple
      @{:tuple (map encode value)}
    :table
      @{:dict (pairs (table ;(map encode (kvs value))))}
    :struct
      @{:dict (pairs (table ;(map encode (kvs value))))}
    other
      (error (string/format "unsupported input: %q" other))))

(varfn decode [] nil)  # mutual recursion idiom

(defn- decode-list
  `a place to debug/extend list decoding`
  [value]
  (case (type value)
    :array value
    :tuple value
    (array ;(decode value))))

(defn- flatten1
  `flatten a list of lists into a single list whatfer table creation`
  [inputs]
  (match inputs
    [[key value] & tail]
    @[key value ;(flatten1 tail)]
    [] @[]))

(defn- decode-ptr
  `currently a no-op`
  [value]
  (let [[scheme path] (string/split ":" value 0 2)]
    (case scheme
      "str"   path
      "int"   (parse path)
      "float" (parse path)
      "bool"  (parse (string/ascii-lower path))
      "none" :null
      (error (string/format "unsupported pointer scheme: %q" scheme)))))

(defn- decode-extension
  `decode a so-called slay "extension" type`
  [js]
  (match js
    {:none value}
      :null
    {:ptr value}
      (decode-ptr (decode value))
    {:str value}
      (string (decode value))
    {:dict [[key value] & rest]}
      (table (decode key) (decode value) ;(flatten1 rest))
    {:list value}
      (array ;(decode value))
    {:tuple value}
      (tuple ;(decode value))
    {:set value}
      (tuple ;(decode value))
    other
      (do
        (print (string/format "unsupported extension: %q" other))
        other)))

(varfn decode
  `decode any json input into native values`
  [js]
  (match (type js)
    :keyword                        # we only support the :null keyword
      (if (= js :null) nil (error (string "unsupported keyword: " js)))
    :number
      js
    :string
      (case js
        "bool:True"
          true
        "bool:False"
          false
        "none:"
          :null
        (decode-ptr js))
    :boolean
      js
    :nil
      nil
    :array                          # arrays are native
      (map decode js)
    :tuple                          # unlikely, but we'll support it
      (tuple ;(map decode js))
    :table                          # slay "extensions"
      (decode-extension js)
    :struct                         # unlikely, but we'll support it
      (decode-extension js)
    other
      (error (string/format "unsupported input: %q" other))))
