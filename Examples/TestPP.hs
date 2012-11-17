{-# OPTIONS_GHC -F -pgmF qapp #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.TestPP where
import QuickAnnotate
 
data D a =
  C1  
  |C2  a  
  |Ann  Loc  (D a)  
  deriving Show
           
instance Annotatable (D a) where annotate = Ann
                         
instance Annotatable ([Char]) where annotate loc d = d ++ (show loc)                          

t = " hello "

test =   C2 1
main = print test