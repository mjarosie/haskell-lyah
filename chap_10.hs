import Control.Monad

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft b (l, r) 
    | abs (l + b - r) < 4 = Right (l + b, r)
    | otherwise = Left ("Pierre slipped with (" ++ show (l + b) ++ ", " ++ show r ++ ")")

landRight :: Birds -> Pole -> Either String Pole
landRight b (l, r)
    | abs (l - (r + b)) < 4 = Right (l, r + b)
    | otherwise = Left ("Pierre slipped with (" ++ show l ++ ", " ++ show (r+b) ++ ")")

x -: f = f x

routine :: Either String Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    Left "Banana!"  
    second <- landRight 2 first  
    landLeft 1 second  

justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x

listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1, 2]
    ch <- ['a', 'b']
    return (n, ch)

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
