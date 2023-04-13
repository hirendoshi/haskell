factorial' :: Integer -> Integer
factorial' n = 
  let f n acc = if n <= 0 then acc else f (n-1) (acc * n)
  in f n 1
