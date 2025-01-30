{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds#-}
{-# LANGUAGE TypeOperators#-}
module Main (main) where

import Criterion.Main

import Pandia.Base
import Pandia.Base.Depth

import Data.PRecord

import qualified Data.PRecord.Internal.PRecord as Cop1
import qualified Data.PRecord.Internal.PRecord2 as Cop2

import Pandia.Space.Geometry
import Pandia.Space.Geometry.Sierpinski
import Pandia.Space.SVG

import Data.Act
import Data.Semigroup.Semidirect
import Control.Monad.Writer
import Data.Semigroup

import Data.Foldable


import qualified Data.Sequence as Seq
import Data.Text.Lazy.Builder

import Data.Proxy

import Graphics.Svg

spiroBench' n = bench (show n) $ nf (renderText . mediaToSVG . spiro') n
spiroBench n = bench (show n) $ nf (renderText . mediaToSVG . spiro) n


foldString n = foldMap (\_ -> "a") (replicate n 1)
foldDual n = foldMap'  (\_ -> Dual "a")  (replicate n 1)

strBench n = bench (show n) $ nf foldString n
strDual n = bench (show n) $ nf foldDual n

type CoprodList =  '[ "Bla" :> Product Int
  , "Blaa" :> Product Int
  , "Blab" :> Product Int
  , "Blac" :> Product Int
  , "Blad" :> Product Int
  , "Blae" :> Product Int
  , "Blaf" :> Product Int
  , "Blag" :> Product Int
  , "Blah" :> Product Int
  , "Blai" :> Product Int
  , "Blaj" :> Product Int
  , "Blak" :> Product Int
  , "Blal" :> Product Int
  , "Time" :> Sum Int -- long list to see performance overhead
  ]

type CoprodBench1 = Cop1.PRecord CoprodList
type CoprodBench2 = Cop2.PRecord CoprodList

mkBench f n = bench (show n) $ nf f n


main :: IO()
main =  defaultMain [
        --   bgroup "Coprod1" (fmap (mkBench $ Cop1.extract #Time . cop1)
        --                         [10, 100,1000,10000,100000])
        -- , bgroup "Coprod2" (fmap (mkBench $ Cop2.extract #Time . cop2)
        --                         [10, 100,1000,10000,100000])
        -- , bgroup "Coprod3" (fmap (mkBench $ Cop3.extract #Time . cop3)
        --                         [10, 100,1000,10000,100000])
        -- , bgroup "Pairs " (fmap (mkBench pairs) [10, 100,1000,10000,100000])
        -- , bgroup "Sum"    (fmap (mkBench sumS) [10, 100,1000,10000,100000])
        --  bgroup "Eq" (fmap (mkBench (\n -> execWriter (unwrapMedia $ spiro n) ==  execWriter (unwrapMedia $ spiro n))) [10,100,1000,10000])
        bgroup "Spiro" (fmap spiroBench [10, 100,1000,10000,100000])
    ]