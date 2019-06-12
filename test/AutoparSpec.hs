module AutoparSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Test.Hspec.QuickCheck
import           Autopar                        ( pFoldMap
                                                , pmap
                                                , pfilter
                                                )
import qualified Data.List                     as List
import           Data.Monoid                    ( Sum )

spec :: Spec
spec = do
    describe "pFoldMap" $
        prop "pFoldMap = Foldable.foldMap" $
            \xs fun -> let f = applyFun fun :: Int -> Sum Int
                        in pFoldMap f xs == foldMap f xs

    describe "pmap" $ do
        prop "pmap = List.map" $
            \xs fun -> let f = applyFun fun :: Int -> Int
                        in List.map f xs == pmap f xs
        prop "functor prop 1: pmap id = id" $
            \xs -> pmap id xs == (xs :: [Int])
        prop "functor prop 2: pmap (g . f) = pmap g . pmap f" $
            \xs fun1 fun2 -> let f = applyFun fun1 :: Int -> Int
                                 g = applyFun fun2 :: Int -> Int
                              in pmap (g . f) xs == (pmap g . pmap f) xs

    describe "pfilter" $
        prop "pfilter = List.filter" $ 
            \xs predFun -> let pred = applyFun predFun :: Int -> Bool
                            in List.filter pred xs == pfilter pred xs