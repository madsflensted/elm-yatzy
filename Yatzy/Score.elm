module Yatzy.Score where


count : List Int -> List (Int, Int)
count ds =
  let numberOf n l =
        l |> List.filter (\x -> x == n) |> List.length
  in [1,2,3,4,5,6]
      |> List.map (\x -> (x, numberOf x ds))


upper : Int -> List Int -> Int
upper n ds =
  ds
  |> List.filter (\x -> x == n)
  |> List.sum


chance : List Int -> Int
chance ds =
  ds
    |> List.sum
  

yatzy : List Int -> Int
yatzy ds =
  case ds of
    h::t -> if List.all (\x -> x == h) t then 50 else 0
    _ -> 0

smallStraight : List Int -> Int
smallStraight ds =
  let sorted = List.sort ds
  in if sorted == [1,2,3,4,5] then 15 else 0

largeStraight : List Int -> Int
largeStraight ds =
  let sorted = List.sort ds
  in if sorted == [2,3,4,5,6] then 20 else 0

fourOfAKind : List Int -> Int
fourOfAKind ds =
  ds
    |> count
    |> List.map (\(n, c) -> if c == 4 then n * 4 else 0)
    |> List.sum

threeOfAKind : List Int -> Int
threeOfAKind ds =
  ds
    |> count
    |> List.map (\(n, c) -> if c == 3 then n * 3 else 0)
    |> List.sum

onePair : List Int -> Int
onePair ds =
  let counts = ds
      |> count
      |> List.filter (\(n, c) -> c == 2)
  in case counts of
      [(n, c)] -> n * c
      _ -> 0

twoPair : List Int -> Int
twoPair ds =
  let counts = ds
      |> count
      |> List.filter (\(n, c) -> c == 2)
  in case counts of
      [(n1, c1), (n2, c2)] -> if n1 /= n2 then (n1 + n2) * 2 else 0 
      _ -> 0

fullHouse : List Int -> Int
fullHouse ds =
  let counts = ds
      |> count
      |> List.filter (\(n, c) -> c == 2 || c == 3)
  in case counts of 
      [(n1, c1), (n2, c2)] -> if c1 /= c2 then n1 * c1 + n2 * c2 else 0 
      _ -> 0
