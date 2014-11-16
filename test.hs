double x = 2*x
quadrauple x = double (double x )
factorial n = product [1..n]

n = a `div` length xs
   where a = 10
         xs = [1,2,3,3,3]

las1t xs  = head ( drop (length xs -1 ) xs )
