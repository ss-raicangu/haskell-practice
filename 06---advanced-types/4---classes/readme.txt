==================================================================================
 https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Type_advanced#Classes
----------------------------------------------------------------------------------
 Accessed at 2020-01-06T11:52:12+1300
==================================================================================



Now we learn how to define our own classes.




_________________________________________________________________________________________________________________________________________________________________________________________________________________________
                                                                                                           Pong
`````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
Based on construction of the game Pong. In the game, there are 2 entities: paddle, ball. While they are different, they share many properties. These can be expressed in terms of a new class.

=========================================================================================================================================================================================================================
 The     Entity     class
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 class Entity a where
     getPosition :: a -> (Int,Int)
     getVelocity :: a -> (Int,Int)
     getAcceleration :: a -> (Int,Int)
     getColor :: a -> Color
     getShape :: a -> Shape
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 Entity     is a typeclass with five methods:     getPosition,     getVelocity,     getAcceleration,     getColor,     getShape.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 Read as "There is a typeclass     'Entity';     a type     'a'     is an instance of     Entity     if it provides the five methods:     getPosition,     getVelocity,     getAcceleration,     getColor,     getShape.
=========================================================================================================================================================================================================================



====================================================
 Defining a     Paddle     data type
----------------------------------------------------
 data Paddle =
    Paddle { paddlePosX, paddlePosY,
             paddleVelX, paddleVelY,
             paddleAccX, paddleAccY :: Int,
             paddleColor :: Color,
             paddleHeight :: Int,
             playerNumber :: Int }

 instance Entity Paddle where
   getPosition p = (paddlePosX p, paddlePosY p)
   getVelocity p = (paddleVelX p, paddleVelY p)
   getAcceleration p = (paddleAccX p, paddleAccY p)
   getColor = paddleColor
   getShape = Rectangle 5 . paddleHeight
====================================================



In our case, many methods of our     Entity     class also need the instances to be instances of     Eq     class. Therefore, we need to make the     Entity     class a sub-class of     Eq     class.

=============================================
 Making     Entity     a sub-class of     Eq
---------------------------------------------
 class Eq a => Entity a where
     getPosition :: a -> (Int,Int)
     getVelocity :: a -> (Int,Int)
     getAcceleration :: a -> (Int,Int)
     getColor :: a -> Color
     getShape :: a -> Shape
---------------------------------------------
 Only the first line changed.
=============================================



After changing the     Entity     class definition, any instances of     Entity     now also have to be instances of     Eq.     This can be derived.




__________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
                                                                                                               Computations
``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
Sometimes, we want computations to fail, e.g., for invalid values. For this section, I'm only posting code. A much better explanation can be read at https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Type_advanced#Computations



data Graph v e = Graph [(Int,v)] [(Int,Int,e)]

search :: Graph v e -> Int -> Int -> Maybe [Int]
search g@(Graph vl el) src dst
    | src == dst = Just [src]
    | otherwise  = search' el
    where search' [] = Nothing
          search' ((u,v,_):es)
              | src == u  =
                case search g v dst of
                  Just p  -> Just (u:p)
                  Nothing -> search' es
              | otherwise = search' es

data Failable a = Success a | Fail String

search2 :: Graph v e -> Int -> Int -> Failable [Int]
search2 g@(Graph vl el) src dst
    | src == dst = Success [src]
    | otherwise  = search' el
    where search' [] = Fail "No path"
          search' ((u,v,_):es)
              | src == u  =
                case search2 g v dst of
                  Success p -> Success (u:p)
                  _         -> search' es
              | otherwise = search' es

search3 :: Graph v e -> Int -> Int -> [[Int]]
search3 g@(Graph vl el) src dst
    | src == dst = [[src]]
    | otherwise  = search' el
    where search' [] = []
          search' ((u,v,_):es)
              | src == u  =
                   map (u:) (search3 g v dst) ++
                   search' es
              | otherwise = search' es

class Computation c where
    success :: a -> c a
    failure :: String -> c a
    augment :: c a -> (a -> c b) -> c b
    combine :: c a -> c a -> c a

instance Computation Maybe where
    success = Just
    failure = const Nothing
    augment (Just x) f = f x
    augment Nothing  _ = Nothing
    combine Nothing y = y
    combine x _ = x

instance Computation Failable where
    success = Success
    failure = Fail
    augment (Success x) f = f x
    augment (Fail s) _ = Fail s
    combine (Fail _) y = y
    combine x _ = x

instance Computation [] where
    success a = [a]
    failure = const []
    augment l f = concat (map f l)
    combine = (++)

searchAll g@(Graph vl el) src dst
    | src == dst = success [src]
    | otherwise  = search' el
    where search' [] = failure "no path"
          search' ((u,v,_):es)
              | src == u  = (searchAll g v dst `augment`
                             (success . (u:)))
                            `combine` search' es
              | otherwise = search' es
