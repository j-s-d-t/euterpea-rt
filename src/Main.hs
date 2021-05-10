{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Euterpea

-- Source sound

sineTable :: Table
sineTable = tableSinesN 512 [1]

-- Apply a window to the source sound

pitches = 
  [ 100, 200, 599, 100 ]

grain :: AudSF () Double
grain = 
  let f = apToHz 65
  in  proc () -> do 
      mod <- oscI sineTable 0 -< 20
      source1 <- oscI sineTable 0 -< (f + (mod * 100))
      outA -< 0.25 * source1

-- Constructing the InstrMap:

-- Single grain:


main :: IO ()
main = 
  rtRender 10000 grain