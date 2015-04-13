module POV (Graph(Graph), fromPOV, tracePathBetween) where

data Graph a = Graph a [Graph a] deriving (Show, Eq)

fromPOV :: a -> Graph a -> Maybe (Graph a)
fromPOV x = (fmap reparent) . (findNode x)

reparent :: GraphZipper a -> Graph a
reparent = undefined

findNode :: a -> Graph a -> Maybe (GraphZipper a)
findNode = undefined

tracePathBetween :: a -> a -> Graph a -> [a]
tracePathBetween = undefined

data GraphZipper a = GraphZipper (Graph a) [Graph a]
