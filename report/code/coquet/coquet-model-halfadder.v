Program Definition HADD a b s c: circuit ( [:a] + [:b]) ([:s] + [:c] ) :=
       Fork2 ([:a] + [:b])
    |> (XOR a b s  &  AND a b c).
