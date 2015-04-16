module POV (Graph(Graph), findNode, fromPOV, tracePathBetween) where

data Graph a = Graph a [Graph a] deriving (Show, Eq)

fromPOV :: Eq a => a -> Graph a -> Maybe (Graph a)
fromPOV x = (fmap reparent) . (findNode x)

reparent :: Eq a => GraphZipper a -> Graph a
reparent (GraphZipper g []) = g
reparent (GraphZipper g gs) = addChild g reparented
    where removeChild (Graph x kids) = Graph x $ filter ((/= (value g)) . value) kids
          addChild (Graph v kids) k = Graph v (kids ++ [k])
          parent = last gs
          withoutKid = removeChild parent
          reparented = reparent (GraphZipper withoutKid (init gs))

findNode :: Eq a => a -> Graph a -> Maybe (GraphZipper a)
findNode sought (Graph x kids)
    | sought == x = Just $ graphZipper here
    | otherwise   = case (findNodeFromKids sought kids) of
        Nothing -> Nothing
        Just (GraphZipper g gs) -> Just $ GraphZipper g (here : gs)
    where here = Graph x kids

findNodeFromKids :: Eq a => a -> [Graph a] -> Maybe (GraphZipper a)
findNodeFromKids _ [] = Nothing
findNodeFromKids sought (k:kids) = case (findNode sought k) of
    Nothing -> findNodeFromKids sought kids
    Just found -> Just found

tracePathBetween :: Eq a => a -> a -> Graph a -> Maybe [a]
tracePathBetween from to g = do
   rerooted <- fromPOV from g
   locOfDest <- findNode to rerooted
   let pathToDest = ((map value) . path) locOfDest
   return $ pathToDest ++ [to]

data GraphZipper a = GraphZipper (Graph a) [Graph a] deriving (Show, Eq)

graphZipper :: Graph a -> GraphZipper a
graphZipper g = GraphZipper g []

path :: GraphZipper a -> [Graph a]
path (GraphZipper _ gs) = gs

value :: Graph a -> a
value (Graph x _) = x
