module Array where

type Array a = [a]

declare :: Int -> Array Int
declare n = replicate n 0

read :: Int -> Array a -> Maybe a
read _ [] = error "Out of bound"
read i (v : vs)
  | i == 0 = Just v
  | i < 0 = error "Out of bound"
  | otherwise = Array.read (i - 1) vs

write :: Int -> a -> Array a -> Maybe (Array a)
write _ _ [] = error "Out of bound"
write i v' (v : vs)
  | i == 0 = Just (v' : vs)
  | i < 0 = error "Out of bound"
  | otherwise = case write (i - 1) v' vs of
                    Just vs' -> Just (v : vs')
                    Nothing -> error "Out of bound"