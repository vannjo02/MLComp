let fun reverse nil = nil
      | reverse (h::t) = (reverse t) @ [h]
in
  println (reverse [1,2,3,4])
end


