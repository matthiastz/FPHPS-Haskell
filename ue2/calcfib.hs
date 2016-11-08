calcFibonacci :: Int -> Int
calcFibonacci i
    | i == 0 = 0
    | i == 1 = 1
    | i > 1 = calcFibonacci (i-1) + calcFibonacci (i-2)

calcPi4 :: Int -> Float
calcPi4 n
    | n == 0 = 1
    | otherwise = (-1)^n / fromIntegral (2 * n + 1) + calcPi4 (n - 1)

calcPi :: Float
calcPi = 4 * calcPi4 1000

ggT :: Int -> Int -> Int
ggT a b
    | a > b = ggT (a - b) b
    | a < b = ggT (b - a) a
    | otherwise  = a

threeEqual :: Int -> Int -> Int -> Bool
threeEqual a b c
    | a == b && b == c = True
    | otherwise = False

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d
    | threeEqual a b c && c == d = True
    | otherwise = False

fourEqual2 :: Int -> Int -> Int -> Int -> Bool
fourEqual2 a b c d
    | a == b && b == c && c == d = True
    | otherwise = False

xor2 :: Bool -> Bool -> Bool
xor2 a b
    | (a || b) && not (a && b) = True
    | otherwise = False

xor3 :: Bool -> Bool -> Bool -> Bool
-- xor3 a b c = a `xor2` b `xor2` c
xor3 a b c = xor2 (xor2 a b) c 

-- Definitionssache (=1) -> Sonderfall 1 1 1 -> True
xor3' :: Bool -> Bool -> Bool -> Bool
xor3' a b c
    | a == False && b == False && c == True = True
    | a == False && b == True && c == False = True
    | a == True && b == False && c == False = True
    | a == True && b == True && c == True = True
    | otherwise = False

middleOfThree :: Int -> Int -> Int -> Int
middleOfThree a b c
    | (a > b && a < c) || (a > c && a < b) = a
    | (b > a && b < c) || (b > c && b < a) = b
    | otherwise = c

-- return how many numbers are equal
howManyEqualOfThree :: Int -> Int -> Int -> Int
howManyEqualOfThree a b c
    | a == b && b == c = 3
    | a == b || b == c || a == c = 2
    | otherwise = 1

howManyEqualOfFour :: Int -> Int -> Int -> Int -> Int
howManyEqualOfFour a b c d
    | a == d || b == d || c == d = 1 + howManyEqualOfThree a b c
    | otherwise = howManyEqualOfThree a b c




