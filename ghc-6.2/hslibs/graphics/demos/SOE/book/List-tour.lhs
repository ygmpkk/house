This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> module PreludeList (
>     map, (++), filter, concat,
>     head, last, tail, init, null, length, (!!), 
>     foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
>     iterate, repeat, replicate, cycle,
>     take, drop, splitAt, takeWhile, dropWhile, span, break,
>     lines, words, unlines, unwords, reverse, and, or,
>     any, all, elem, notElem, lookup,
>     sum, product, maximum, minimum, concatMap, 
>     zip, zip3, zipWith, zipWith3, unzip, unzip3)
>   where

> import qualified Char(isSpace)

> infixl 9  !!
> infixr 5  ++
> infix  4  `elem`, `notElem`

> head             :: [a] -> a
> head (x:_)       =  x
> head []          =  error "PreludeList.head: empty list"
> 
> last             :: [a] -> a
> last [x]         =  x
> last (_:xs)      =  last xs
> last []          =  error "PreludeList.last: empty list"
> 
> tail             :: [a] -> [a]
> tail (_:xs)      =  xs
> tail []          =  error "PreludeList.tail: empty list"
> 
> init             :: [a] -> [a]
> init [x]         =  []
> init (x:xs)      =  x : init xs
> init []          =  error "PreludeList.init: empty list"

> null             :: [a] -> Bool
> null []          =  True
> null (_:_)       =  False

> (!!)                :: [a] -> Int -> a
> (x:_)  !! 0         =  x
> (_:xs) !! n | n > 0 =  xs !! (n-1)
> (_:_)  !! _         =  error "PreludeList.!!: negative index"
> []     !! _         =  error "PreludeList.!!: index too large"

> take                   :: Int -> [a] -> [a]
> take 0 _               =  []
> take _ []              =  []
> take n (x:xs) | n > 0  =  x : take (n-1) xs
> take _     _           =  error "PreludeList.take: negative argument"

> drop                   :: Int -> [a] -> [a]
> drop 0 xs              =  xs
> drop _ []              =  []
> drop n (_:xs) | n > 0  =  drop (n-1) xs
> drop _     _           =  error "PreludeList.drop: negative argument"

> splitAt                  :: Int -> [a] -> ([a],[a])
> splitAt 0 xs             =  ([],xs)
> splitAt _ []             =  ([],[])
> splitAt n (x:xs) | n > 0 =  (x:xs',xs'') 
>                             where (xs',xs'') = splitAt (n-1) xs
> splitAt _     _          =  error "PreludeList.splitAt: negative argument"

> length           :: [a] -> Int
> length []        =  0
> length (_:l)     =  1 + length l

| take    3 [0, 1 .. 5] ==> [0,1,2]
| drop    3 [0, 1 .. 5] ==> [3,4,5]
| splitAt 3 [0, 1 .. 5] ==> ([0,1,2],[3,4,5])

> takeWhile               :: (a -> Bool) -> [a] -> [a]
> takeWhile p []          =  []
> takeWhile p (x:xs) 
>             | p x       =  x : takeWhile p xs
>             | otherwise =  []
> 
> dropWhile               :: (a -> Bool) -> [a] -> [a]
> dropWhile p []          =  []
> dropWhile p xs@(x:xs')
>             | p x       =  dropWhile p xs'
>             | otherwise =  xs
> 
> span, break             :: (a -> Bool) -> [a] -> ([a],[a])
> span p []               =  ([],[])
> span p xs@(x:xs') 
>             | p x       =  (x:xs',xs'') where (xs',xs'') = span p xs
>             | otherwise =  (xs,[])
>
> break p                 =  span (not . p)

> filter :: (a -> Bool) -> [a] -> [a]
> filter p [] = []
> filter p (x:xs) | p x       = x : filter p xs
>                 | otherwise = filter p xs

> foldl            :: (a -> b -> a) -> a -> [b] -> a
> foldl f z []     =  z
> foldl f z (x:xs) =  foldl f (f z x) xs

> foldl1           :: (a -> a -> a) -> [a] -> a
> foldl1 f (x:xs)  =  foldl f x xs
> foldl1 _ []      =  error "PreludeList.foldl1: empty list"

> foldr            :: (a -> b -> b) -> b -> [a] -> b
> foldr f z []     =  z
> foldr f z (x:xs) =  f x (foldr f z xs)

> foldr1           :: (a -> a -> a) -> [a] -> a
> foldr1 f [x]     =  x
> foldr1 f (x:xs)  =  f x (foldr1 f xs)
> foldr1 _ []      =  error "PreludeList.foldr1: empty list"

| scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]

| scanl (+) 0 [1,2,3]  ==>  [0,1,3,6]

| scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]

> scanl            :: (a -> b -> a) -> a -> [b] -> [a]
> scanl f q xs     =  q : (case xs of
>                             []   -> []
>                             x:xs -> scanl f (f q x) xs)
> scanl1           :: (a -> a -> a) -> [a] -> [a]
> scanl1 f (x:xs)  =  scanl f x xs
> scanl1 _ []      =  error "PreludeList.scanl1: empty list"
> 
> scanr             :: (a -> b -> b) -> b -> [a] -> [b]
> scanr f q0 []     =  [q0]
> scanr f q0 (x:xs) =  f x q : qs
>                      where qs@(q:_) = scanr f q0 xs 
> 
> scanr1           :: (a -> a -> a) -> [a] -> [a]
> scanr1 f  [x]    =  [x]
> scanr1 f  (x:xs) =  f x q : qs
>                     where qs@(q:_) = scanr1 f xs 
> scanr1 _ []      =  error "PreludeList.scanr1: empty list"

| iterate f x  ==>  [x, f x, f (f x), ...]

> iterate          :: (a -> a) -> a -> [a]
> iterate f x      =  x : iterate f (f x)

> repeat           :: a -> [a]
> repeat x         =  xs where xs = x:xs
>
> replicate        :: Int -> a -> [a]
> replicate n x    =  take n (repeat x)
>
> cycle            :: [a] -> [a]
> cycle []         = error "Prelude.cycle: empty list" 
> cycle xs         =  xs' where xs' = xs ++ xs'

> lines            :: String -> [String]
> lines ""         =  []
> lines s          =  let (l, s') = break (== '\n') s
>                       in  l : case s' of
>                                 []      -> []
>                                 (_:s'') -> lines s''
> 
> words            :: String -> [String]
> words s          =  case dropWhile Char.isSpace s of
>                       "" -> []
>                       s' -> w : words s''
>                             where (w, s'') = break Char.isSpace s'
> 
> unlines          :: [String] -> String
> unlines          =  concatMap (++ "\n")
> 
> unwords          :: [String] -> String
> unwords []       =  ""
> unwords ws       =  foldr1 (\w s -> w ++ ' ':s) ws

> reverse          :: [a] -> [a]
> reverse          =  foldl (flip (:)) []

> and, or          :: [Bool] -> Bool
> and              =  foldr (&&) True
> or               =  foldr (||) False

> any, all         :: (a -> Bool) -> [a] -> Bool
> any p            =  or . map p
> all p            =  and . map p

> elem, notElem    :: (Eq a) => a -> [a] -> Bool
> elem x           =  any (== x)
> notElem x        =  all (/= x)

> lookup           :: (Eq a) => a -> [(a,b)] -> Maybe b
> lookup key []    =  Nothing
> lookup key ((x,y):xys)
>     | key == x   =  Just y
>     | otherwise  =  lookup key xys

> sum, product     :: (Num a) => [a] -> a
> sum              =  foldl (+) 0  
> product          =  foldl (*) 1

> maximum, minimum :: (Ord a) => [a] -> a
> maximum []       =  error "Prelude.maximum: empty list"
> maximum xs       =  foldl1 max xs
>
> minimum []       =  error "Prelude.minimum: empty list"
> minimum xs       =  foldl1 min xs

> map :: (a -> b) -> [a] -> [a]
> map f []     = []
> map f (x:xs) = f x : map f xs

> (++) :: [a] -> [a] -> [a]
> []     ++ ys = ys
> (x:xs) ++ ys = x : (xs ++ ys)

> concat :: [[a]] -> [a]
> concat xss = foldr (++) [] xss

> concatMap        :: (a -> [b]) -> [a] -> [b]
> concatMap f      =  concat . map f

> zip              :: [a] -> [b] -> [(a,b)]
> zip              =  zipWith (,)
> 
> zip3             :: [a] -> [b] -> [c] -> [(a,b,c)]
> zip3             =  zipWith3 (,,)

| (,)   ==>  \x y ->   (x,y)
| (,,)  ==>  \x y z -> (x,y,z)

> zipWith          :: (a->b->c) -> [a]->[b]->[c]
> zipWith z (a:as) (b:bs)
>                  =  z a b : zipWith z as bs
> zipWith _ _ _    =  []
> 
> zipWith3         :: (a->b->c->d) -> [a]->[b]->[c]->[d]
> zipWith3 z (a:as) (b:bs) (c:cs)
>                  =  z a b c : zipWith3 z as bs cs
> zipWith3 _ _ _ _ =  []

> unzip            :: [(a,b)] -> ([a],[b])
> unzip            =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])
> 
> unzip3           :: [(a,b,c)] -> ([a],[b],[c])
> unzip3           =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
>                           ([],[],[])

