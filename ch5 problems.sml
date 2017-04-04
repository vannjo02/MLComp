(* #10 *)
fun gcd(x,y)=
    if y = 0 then x
    else gcd(y,x mod y)



(* #11 *)
fun allCaps(s)=
    if s = "" then ""
    else implode(Char.toUpper(String.sub(s,0)):: explode((allCaps(String.substring(s,1,String.size(s)-1)))))


(* #12 *)
fun firstCaps(L)=
    if L= [] then []
    else implode((Char.toUpper(String.sub(hd(L),0)))::tl(explode(hd(L)))) :: (firstCaps(tl(L)))


(* #13 *)
fun swap([])=[]
  | swap([x]) = [x]
  | swap(x::y::t) = y::x::swap(t) 


(* #14 *)
fun rotate(0,L) = L
   | rotate(n,[]) = []
   | rotate (n,h::t) = rotate ((n-1),(t@[h]))


(* #15 *)
fun delete(n,s) = 
   let fun delhelper(n,[])=[]
          | delhelper(0,h::t)=t
          | delhelper(n,h::t)= h::(delhelper(n-1,t))
    in
      implode(delhelper(n,explode(s)))
    end
   

(* #16 *)
fun intpow(0,n)= 0
  | intpow (x,0) = 1
  | intpow (x,n) = x*(intpow(x,n-1))


(* #17 *)
fun rotate2(n,L)=
     let fun rotatehelper(0,t,acc) = t@acc
           | rotatehelper(n,[],acc)=rotatehelper(n-1,acc,[])
           | rotatehelper(n,h::t,acc) = rotatehelper(n-1,t,acc@[h])
      in 
         rotatehelper(n,L,[])
      end 


(* #18 *)
fun rotate3(n,[])=[]
   | rotate3(n,L) = 
   let fun rot3helper(0,t,acc) = t@acc
          | rot3helper(n,[],acc)=rot3helper(n-1,acc,[])
          | rot3helper(n,h::t,acc) = rot3helper(n-1,t,acc@[h])
    in 
      rot3helper((n mod List.length(L)),L,[])
    end


(* #19 *)
fun delete2 n s = 
   let fun delhelper(n,[])=[]
          | delhelper(0,h::t)=t
          | delhelper(n,h::t)= h::(delhelper(n-1,t))
    in
      implode(delhelper(n,explode(s)))
    end


(* #20 *)
fun delete5(s)=
    if String.size(s) < 5 then s
    else implode((List.take(explode(s),5))@ (List.drop(explode(s),6)))

(* #21 *)
fun evens(L)=
   List.filter (fn x => (x mod 2 )= 0) L


(* #22 *)
fun lowerFirsts(L)=
   List.filter (fn x => Char.isLower(String.sub(x,0)) = true ) L


(* #23 *)
fun allCaps2(s) = 
    (*if s ="" then "" else*)
    implode(map (fn x => Char.toUpper(x)) (explode s))
  

(* #24 *)
(*  Doesn't work


fun find(s,file)=
   let fun fhelper(afile,aString)=
              let val line=TextIO.inputLine afile
               in
                if line= NONE then ()
                else
                   let val SOME s = line
                   in
                    if .....
                     then TextIO.output(TextIO.stdOut,s)
                   

                  
                  end 
               end
   in 
   while (not (TextIO.endOfStream file)) do (
     fhelper(TextIO.openIn file, s)
      )
   end
*)


(* #25 *)

fun transform f L =    
   let fun transhelper [] = []        
         | transhelper (h::t) = (f h)::(transhelper t) handle _ => h::(transhelper t)

   in 
     (transhelper L) 
    end
    


(* #26  *)
datatype 
    natural = O
       | succ of natural 


(* #27 *)
fun convert(O) = 0
 | convert(succ(x)) = 1 + convert(x)



(*  #28 *)
fun add(O,x) = x
  | add(y,O) = y
  | add (x,succ(y))= succ(add(x,y))



(* #29 *)
fun mul(x,O)= O
 | mul (O,y) = O
 | mul (x,succ(O)) = x
 | mul (succ(O), y) = y
 | mul(x,succ(y)) = add(x, mul(x,y))


(* #30 *)
fun hadd(L) = 
  List.foldr (fn (x,y) => add(x,y)) O L


       







   



