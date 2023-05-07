module Cantor where

import H_Base
import Transformations
import Data.List (find)

type Encoding = [Integer]
type CandidateCovering = [Shape]

-- Convert a binary sequence to a shape
decodeShape :: Encoding -> Shape
decodeShape [] = []
decodeShape (x : y : rest) = point : decodeShape rest
  where
    point = (decodeBinary (x : takeWhile (== 0) rest), decodeBinary (y : takeWhile (== 1) rest))
    decodeBinary = go sternBrocot
    go (Node p left right) bs = case bs of
      [] -> p
      b : bs' -> if b == 0 then go left bs' else go right bs'

isSubset :: Shape -> CandidateCovering -> Bool
isSubset shape covering = any (\coveringShape -> all (`elem` coveringShape) shape) covering

isCovered :: Shape -> CandidateCovering -> Bool
isCovered shape candidateCovering =
  any (\transformation -> isSubset (applyTransformation transformation shape) candidateCovering) allTransformations

isValidUniversalCovering :: CandidateCovering -> [Encoding] -> Bool
isValidUniversalCovering covering encodings = all checkCoverage encodings
  where
    checkCoverage encoding = case find (\shape -> isCovered shape covering) (shapesFromEncoding encoding) of
      Nothing -> False
      Just _ -> True

    shapesFromEncoding encoding = map (\t -> applyTransformation t (decodeShape encoding)) allTransformations


applyTransformationList :: [Transformation] -> Shape -> Shape
applyTransformationList ts s = foldr applyTransformation s ts

main :: IO ()
main = do
  let candidateCovering = [[(0, 0), (1, 0), (1, 1), (0, 1)]]
  let encodings = [encodeShape [(0, 0), (1, 0), (1, 1), (0, 1)]]
  putStrLn $ "Is the candidate covering valid? " ++ show (isValidUniversalCovering candidateCovering encodings)
