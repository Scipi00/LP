fizzBuzz = map (mod_3_5) [0..]
    where mod_3_5 :: Int -> Either Int String
          mod_3_5 x
              | ((mod x 3) == 0) && ((mod x 5) == 0) = Right "FizzBuzz"
              | ((mod x 3) == 0) =  Right "Fizz"
              | ((mod x 5) == 0) = Right "Buzz"
              | otherwise = Left x
