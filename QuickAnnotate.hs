{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- | QuickAnnotate Module
module QuickAnnotate where 

-- | Type of source location data
type Loc = String           

-- |'Annotatable' is used for overloading the annotate function added by the preprocessor.
class Annotatable a  where        
  annotate :: Loc -> a -> a 

-- | By default all types that do not derive 'Annotatable' expilicitly are annotated by 'id'.   
instance Annotatable a where
  annotate _ =  id

-- | The 'annotate' function ignores types of the arguments in a function (abstraction) and  
-- annotates the returned value based on its type. For example:   
-- 'annotate ( \x y -> x )' is equal to '\x y -> annotate x'  
instance Annotatable b => Annotatable (a -> b) where
  annotate l f = \x -> annotate l (f x)  

