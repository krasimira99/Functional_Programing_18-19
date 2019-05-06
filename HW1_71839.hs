main :: IO()
main = do
 print(solveQuadratic 3 4 5)
 
--Задача 1
solveQuadratic::Double->Double->Double->(Double, Double)
 solveQuadratic a b c
 | b*b-4*a*c < 0 = error "The quadratic equation has no solution"
 | otherwise     =((((-b)-sqrt(b*b-4*a*c)/(2*a)),((-b)+sqrt(b*b-4*a*c)/(2*a))))