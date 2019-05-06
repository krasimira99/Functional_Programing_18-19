main::IO()
main=do
  
--Task1
pairCompose::[Int->Int]->(Int->Int)
pairCompose [] x = 0 
pairCompose (f:g:fs) x = firstTwo+pairComp fs x
 where 
  firstTwo =(f.g)x
  pairComp (f:fs) x = (f.(\x->x)) x
  
--Task2
myFunc::(Int->Int)->(Int->Int)->Int->(Int->Int)
myFunc f g 0 x = 0 
myFunc f g 1 x = f x
myFunc f g n x = if even n then (g((myFunc f g (n-1)) x)) else (f((myFunc f g (n-1))x))

switchsum::(Int->Int)->(Int->Int)->Int->(Int->Int)
switchsum f g 0 x = 0
switchsum f g n x = (myFunc f g n x)+(switchsum f g (n-1) x)

--Task3
type K = (Int,Int)
k::K
k=(1,5)

replaceAssoc::[Int]->[K]->[Int]
replaceAssoc [] _ = []
replaceAssoc list [] = list
replaceAssoc (x:list) dict = repl:(replaceAssoc list dict)
 where 
  repl = if null[n|(m,n)<-dict,m==x] then x else (head[n|(m,n)<-dict,m==x])