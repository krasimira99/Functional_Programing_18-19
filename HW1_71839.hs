main::IO()
main=do
 
--Task1
solveQuadratic::Double->Double->Double->(Double,Double)
solveQuadratic a b c
 |b*b-4*a*c<0 =error "No solution"
 |otherwise = ((((-b)+sqrt(b*b-4*a*c))/(2*a)),(((-b)-sqrt(b*b-4*a*c))/(2*a)))

--Help function
isPrime::Integer->Integer->Bool
isPrime n number
 |number>(div n 2) =True
 |mod n number==0 =False
 |otherwise = isPrime n (number+1)
 
--Task2
sumPrimes::Integer->Integer->Integer
sumPrimes n k
 |k==0 =0
 |(isPrime n 2) = n+ (sumPrimes (n+1) (k-1))
 |otherwise = sumPrimes (n+1) k
 
--Task3
countPalyndromes::Integer->Integer->Integer
countPalyndromes a b
 |a==b = 0
 |(isPalyndrome a) = 1+ (countPalyndromes (a+1) b)
 |otherwise = (countPalyndromes (a+1) b)
 where
  isPalyndrome n =  n==(reverseNum n 0)
  reverseNum n result
   |n<10 = result*10+n
   |otherwise = reverseNum (div n 10) (result*10+(mod n 10))
   
--Task4
truncatablePrime::Integer->Bool
truncatablePrime n
 |n==0                   = True
 |(isPrime n 2) == False = False
 |otherwise              = True && (truncatablePrime(div n 10))