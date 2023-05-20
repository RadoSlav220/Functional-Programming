import Data.Char

main :: IO()
main = do
    print (willItFly [1, 4, 2, 3])
    print (willItFly [1, 4, 2, -1, 6])
    
    print (formatDuration 0)
    print (formatDuration 1)
    print (formatDuration 62)
    print (formatDuration 120)
    print (formatDuration 3600)
    print (formatDuration 3662)
    print (formatDuration 7386)
    print (formatDuration 100000)
    print (formatDuration 200000)
    print (formatDuration 32000000)
    print (formatDuration 68000000)
    print (formatDuration 67999980)
    print (formatDuration 67809200)
    print (formatDuration 107806020)

--Task 1
willItFly :: [Int] -> Bool
willItFly xs = helper xs (length xs)
    where
        helper [] n = True
        helper [_] n = True
        helper (x : y : ys) n = abs (x - y) < n && (helper (y : ys) n)

--Task 2
getSeconds :: Int -> (Int, String)
getSeconds x = (x `mod` 60, "second")

getMinutes :: Int -> (Int, String)
getMinutes x = ((x `div` 60) `mod` 60, "minute")

getHours :: Int -> (Int, String)
getHours x = ((x `div` 3600) `mod` 24, "hour")

getDays :: Int -> (Int, String)
getDays x = ((x `div` 86400) `mod` 365, "day")

getYears :: Int -> (Int, String)
getYears x = (x `div` 31536000, "year")

formatDuration :: Int -> String
formatDuration x = if (len > 0) then createString timeList len else "now" 
        where
            timeList = [i | i <- [(getYears x), (getDays x), (getHours x), (getMinutes x), (getSeconds x)], fst i > 0]
            len = length timeList
            
            createString lst rem 
                | len > 1 && rem == 1 && fst (head lst) > 1   = "and " ++ (show (fst (head lst))) ++ ' ' : snd (head lst) ++ "s"
                | len > 1 && rem == 1 && fst (head lst) == 1  = "and " ++ (show (fst (head lst))) ++ ' ' : snd (head lst) 
                | len == 1 && rem == 1 && fst (head lst) > 1  = (show (fst (head lst))) ++ ' ' : snd (head lst) ++ "s"
                | len == 1 && rem == 1 && fst (head lst) == 1 = (show (fst (head lst))) ++ ' ' : snd (head lst)
                | rem >= 3 && fst (head lst) > 1              = show (fst (head lst)) ++ ' ' : snd (head lst) ++ "s, " ++ createString (tail lst) (rem - 1) 
                | rem >= 3 && fst (head lst) == 1             = show (fst (head lst)) ++ ' ' : snd (head lst) ++ ", " ++ createString (tail lst) (rem - 1)
                | fst (head lst) > 1                          = show (fst (head lst)) ++ ' ' : snd (head lst) ++ "s " ++ createString (tail lst) (rem - 1) 
                | otherwise                                   = show (fst (head lst)) ++ ' ' : snd (head lst) ++ ' ' : createString (tail lst) (rem - 1)  