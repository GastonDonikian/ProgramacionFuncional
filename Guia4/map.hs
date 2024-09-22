type Map k v = (k -> Maybe v)

lookupM :: Eq k => k -> Map k v -> Maybe v
emptyM :: Map k v
assocM :: Eq k => k -> v -> Map k v -> Map k v
deleteM :: Eq k => k -> Map k v -> Map k v

lookupM k m = m k

ejemploMap :: Map Int Int
ejemploMap 2 = Just 4
ejemploMap 1 = Just 2
ejemploMap _ = Nothing

emptyM = \x -> Nothing

assocM k v m = \k1 -> if k == k1 then Just v else m k1

deleteM k m = \k1 -> if k == k1 then Nothing else m k1

