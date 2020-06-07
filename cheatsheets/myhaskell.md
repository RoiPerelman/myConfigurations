Roi's haskell
=============

## haskell is a **Purely Functional Programming** language- 
* declarative programming paradigm - **NO variables**, **Only Experssions**.
* when there are no variables there is no state to change, function have **NO Side Effects**
* referential transparency - pure functions **output depends solely on their input**
* **functional functions are ones we can memoize** - only calculate if we didn't in the past.

## haskell is **Lazy** (as opposed to eager and memoized)
* for example infinite list [1,2..] is only evaluated when needed

## haskell is **strongly statically typed** with **type inference**
* automatic detection of the data type of an expression

## ghci - glasgow haskell compiler
* `ghci 2>&1 | HsColour -tty` - open ghci with colour
* :set prompt "haskell> "
* :l <file path> (name ends with .hs)
* :m <module name>

## stack - like pip/npm/sbt etc'
* `stack setup` - makes a local copy of ghc
* `stack execute #{executable}` - usually the same name as the project
* `stack test`
* `stack ghci` - loads all library to ghci

## Association
* haskell is left associative meaning:
    * `f a b c` is the same as `(((f a) b) c)`
* to fix that we can use `$` which makes stuff right assosiative
    * `f $ a $ b $ c`
``` haskell
ghci> sqrt 3 + 4 + 9
14.732050807568877
ghci> sqrt $ 3 + 4 + 9
4.0
```

## Parametricity
* It is more about modularity and correctness than it is about generalizing for code reuse
* why should we use parametrization and not specific types? isn't it overgeneralizing?
    * No becuase otherwise we expose too much information to the function that we cannot reason about when looking at the function signature
    * parametric definition isn't just for code reuse. We are also limiting the amount of data we need to keep in our head in order to work on this function
    * nice twitter account to learn https://twitter.com/parametricity?lang=en

## Functions
* Functions use Arrow notation
* Functions are curried by default
``` haskell
-- two ways to create functions:
-- with parameters
add3 x y z = x + y + z
-- with lambda functions
add3_2 = \x -> \y -> \z -> x + y + z
```

## Composition of functions
``` haskell
-- haskell has a nice notation to use composition with `.`
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x) 

-- example (with lambda function)
ghci> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  
[-5,-3,-6,-7,-3,-2,-19,-24]  
ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  
[-5,-3,-6,-7,-3,-2,-19,-24]  
```

## if else and guarded notation (can be skipped)
```haskell
signum :: Int -> Int
signum n = if n < 0 then -1 else
             if n == 0 then 0 else 1

abs n | n > 0 = n
      | otherwise = -n
``` 

## pattern matching
```haskell
type Bool = True | False

negate :: Bool -> Bool
negate True = False
negate False = True

sayNumUpTo5 :: (Integral a) => a -> String
sayNumUpTo5 1 = "One"
sayNumUpTo5 2 = "Two"
sayNumUpTo5 3 = "Three"
sayNumUpTo5 4 = "Four"
sayNumUpTo5 5 = "Five"
sayNumUpTo5 _ = "Not between 1 and 5!"
```

## Type Synonyms
``` haskell
-- String is just an array of chars
type String = [Char]
-- lets define a phonebook type
type PhoneBook = [(String, String)]
-- Its not exectly understandable yet. What are those Strings?
-- lets make it better
type PhoneNumber = String
type Name = String
-- remember haskell is a pure language, can't redefine types/ variables
type PhoneBook2 = [(Name,PhoneNumber)]
-- much better!
```

## Data Types
``` haskell
-- algebraic data types
-- Lets make new Types with type constructors:
data Bool = False | True
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- Circle and Rectangle are data constructors
-- for example circle type is:
-- Circle :: Float -> Float -> Float -> Shape
-- The deriving keyword is a way to inherit properties from a class. 
-- It is needed if we want to print, More about that later 

-- Record syntex: 
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show, Read, Eq)
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- type parameters
data Maybe a = Nothing | Just a
-- where Maybe is a type constructor and a is the parameter

-- recursive data types structures
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
```

## Type Classes
``` haskell
-- kind of like interface/ trait, used for inheritance
class Eq a where  
(==) :: a -> a -> Bool  
(/=) :: a -> a -> Bool  
x == y = not (x /= y)  
x /= y = not (x == y)

-- if we had a data type of traffic light
data TrafficLight = Red | Yellow | Green
-- we could derive if we had a basic implementation in the class
-- orrrrr
-- we could just make an instance of Eq for TrafficLight
instance Eq TrafficLight where  
Red == Red = True  
Green == Green = True  
Yellow == Yellow = True  
_ == _ = False

-- full example:
import Control.Monad

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

-- a class can inherit from another class
-- here is a definition of Ord class
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a
-- Ord class inherits from eq class
class  (Num a, Ord a) => Real a  where
    -- | the rational equivalent of its real argument with full precision
    toRational          ::  a -> Rational
-- Real class inherits form Num and Ord class
```

## NewType (can be skipped)
``` haskell
-- newtype - a lazier version of Type that is restricted to just one constructor with one field
data CoolBool = CoolBool { getCoolBool :: Bool }
helloMe :: CoolBool -> String  
helloMe (CoolBool _) = "hello"

newtype CoolBool2 = CoolBool2 { getCoolBool2 :: Bool }
helloMe2 :: CoolBool2 -> String  
helloMe2 (CoolBool2 _) = "hello"

-- with newtype the compiler doesnt need to find the type for the function, it knows it can only be a Bool
ghci> helloMe undefined  
"*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:3:9 in interactive:Ghci3"
ghci> helloMe2 undefined  
"hello"
```

* type vs newtype vs data
    * type for making synonyms : `type IntList = [Int]` can be used like `([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])`
    * The newtype keyword is for taking existing types and wrapping them in new types, mostly so that it's easier to make them instances of certain type classes. When we use newtype to wrap an existing type, the type that we get is separate from the original type. If we make the following newtype : `newtype CharList = CharList { getCharList :: [Char] }`
    * The data keyword is for making your own data types and with them, you can go hog wild. They can have as many constructors and fields as you wish and can be used to implement any algebraic data type by yourself. Everything from lists and Maybe-like types to trees.
* summary : If you just want your type signatures to look cleaner and be more descriptive, you probably want type synonyms. If you want to take an existing type and wrap it in a new type in order to make it an instance of a type class, chances are you're looking for a newtype. And if you want to make something completely new, odds are good that you're looking for the data keyword.

## derivitives (deriving for types)
* Eq
* Ord
* Enum
* Bounded
* Show
* Read

## list comprehension (can be skipped)
``` haskell
-- [result for each input | input A <-, input B <- ..., filter A > ?, filter B

-- examples:
-- a version of length
ghci> sum [1 | _ <- [1,2,3,4,5]]
5
-- remove all except upper case
ghci> [ c | c <- "AbCdE", c `elem` ['A'..'Z']]
"ACE"
-- get all products of 2 lists that are above 50
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]  
[55,80,100,110]
```

## Where & Let in scoping (can be skipped)
``` haskell
-- both are they same except scoping in guard notation. 
-- where is for all guards wheres let in is individual
-- basically choose one you prefer and use it
-- i like let in :)
multiples x = 
    map mult [1..10]
    where mult n = n * x

multiples2 x =
    let mult n = n * x
    in map mult [1..10]

listStats m =
    "total: " ++ show total ++ ", avg: " ++ show avg
    where numbers = [1 .. m]
          total = sum numbers
          avg = total / m

listStats2 m =
    let numbers = [1 .. m]
        total = sum numbers
        avg = total / m
    in "total: " ++ show total ++ ", avg: " ++ show avg
```

## Functors
things we can map over
``` haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b

(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x

instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  

-- maybe example:
fmap (*3) (Just 5)
-- <$> is fmap infix operator
(*3) <$> Just 5

-- Functor laws
-- Functors must preserve identity morphisms
fmap id = id
-- Functors preserve composition of morphisms
fmap (f . g) F = fmap f (fmap g F)
```

## Applicative Functors (can be skipped)
adding functionality to function with a way to map over with applicative mapping functions 
``` haskell
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  

-- Maybe example:

--  Applicative functors are there if we want to use as a mapping function (Just *3) instead *3 to map over
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something

-- three examples that are all the same:
-- normal functor properties usage (fmap)
fmap (++) (Just "johntra") <*> Just "volta"
-- inline data type of fmap = <$>
(++) `fmap` Just "johntra" <*> Just "volta"
(++) <$> Just "johntra" <*> Just "volta"
-- applicative functor usage
Just (++) <*> Just "johntra" <*> Just "volta"

-- List [] example:

instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]

ghci> pure "Hey" :: [String]  
["Hey"]  
ghci> pure "Hey" :: Maybe String  
Just "Hey"
ghci> [(*0),(+100),(^2)] <*> [1,2,3]  
[0,0,0,101,102,103,1,4,9]  
ghci> [(+),(*)] <*> [1,2] <*> [3,4]  
[4,5,5,6,3,4,6,8]

-- IO example:

instance Applicative IO where  
pure = return  
a <*> b = do  
    f <- a  
    x <- b  
    return (f x)  

myAction :: IO String  
myAction = (++) <$> getLine <*> getLine  
main = do  
a <- (++) <$> getLine <*> getLine  
putStrLn $ "The two lines concatenated turn out to be: " ++ a  
```

## Monoid typeclass
*, ++ are monoids because they both have identity values and are associative. (1 and [])
** associative means that (1 * 2) * 3 = 1 * (2 * 3)
``` haskell
-- 
class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty

-- this infix synonym for mappend is found in Data.Monoid
x <> y = mappend x y

-- Monoid laws:
-- Identity law (<> is like `mappend`)
-- x <> mempty = x
-- mempty <> x = x
-- Associativity
-- (x <> y) <> z = x <> (y <> z)
```

## Monad typeclass
adding functionality to functors with ability to map over with function that takes a normal input and outputs a functor
``` haskell
class Monad m where  
    return :: a -> m a  
  
    (>>=) :: m a -> (a -> m b) -> m b  
  
    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  
  
    fail :: String -> m a  
    fail msg = error msg  

-- maybe example

instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing  

-- List example

instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  
    fail _ = []  

-- 3 ways to use the monad:
-- bind operator (monadic experssion)
ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)  
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- do notation
listOfTuples :: [(Int,Char)]  
listOfTuples = do  
n <- [1,2]  
ch <- ['a','b']  
return (n,ch)

-- list comprehension
ghci> [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]  
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]  

-- Monad laws:

-- left identitiy - return x >>= f is the same damn thing as f x
-- (return x) >>= f == f x
ghci> return 3 >>= (\x -> Just (x+100000))  
Just 100003  
ghci> (\x -> Just (x+100000)) 3  
Just 100003  

-- right identity - m >>= return is no different than just m
-- m >>= return == m
ghci> Just "move on up" >>= (\x -> return x)  
Just "move on up"  
ghci> [1,2,3,4] >>= (\x -> return x)  
[1,2,3,4]  
ghci> putStrLn "Wah!" >>= (\x -> return x)  
Wah!

-- associativity - Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)
-- (m >>= f) >>= g == m >>= (\x -> f x >>= g)
((return (0,0) >>= landRight 2) >>= landLeft 2) >>= landRight 2  
return (0,0) >>= (\x -> 
landRight 2 x >>= (\y -> 
landLeft 2 y >>= (\z -> 
landRight 2 z)))  
```

## do notation
``` haskell
import Data.Maybe

-- do notation uses bind >>= with nicer syntax
greetingStr :: Maybe String
greetingStr = do
   str1 <- Just "Hello "
   str2 <- Just "World"
   Just (str1 ++ str2)

-- bind >>= is like `flatMap` for monads
greetingStr2 :: Maybe String
greetingStr2 =
   Just "Hello " >>= \str1 ->
   Just "World" >>= \str2 ->
   Just (str1 ++ str2)

-- if there is no backwards arrow <-, means we ignore the result of the function
greetingAction :: IO ()
greetingAction = do
   putStr "Hello "
   putStr "World"
   putStrLn ""

-- then >> operand is like bind >>= except it ignores input
greetingAction :: IO ()
greetingAction = 
    putStr "Hello " >>
    putStr "World" >>
    putStrLn ""
```