-- Andrey O. Matveev
-- After Remarks A.1 and A.2 given in the monograph A.O. Matveev, Symmetric Cycles: A 2D Perspective
-- on Higher Dimensional Discrete Hypercubes, the Power Sets of Finite Sets, and Set Families,
-- Leanpub, 2022, https://leanpub.com/SymmetricCycles.
--
-- A Smirnov word is defined to be a word whose adjacent letters always differ.
--
-- In the theoretical framework of the breakthrough article
--
-- Helmut Prodinger, Ternary Smirnov words and generating functions. Integers, 2018, 18, Paper A69,
-- http://math.colgate.edu/~integers/vol18.html,
--
-- below we calculate the numbers of Smirnov words over three-letter and four-letter alphabets.

module SmirnovWordsModule
  ( numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet
  , numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet
  , numberOfThetaThetaSmirnovWordsOverFourLetterAlphabet
  , numberOfThetaAlphaSmirnovWordsOverFourLetterAlphabet
  ) where

import           Math.Combinatorics.Exact.Binomial (choose)

type ParikhVector3 = (Integer, Integer, Integer) -- Parikh vectors for the three-letter alphabet $(\theta, \alpha, \beta)$

type ParikhVector4 = (Integer, Integer, Integer, Integer) -- Parikh vectors for the four-letter alphabet $(\theta, \alpha, \beta, \gamma)$

numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet ::
     ParikhVector3 -> Integer
-- Name means "To get (quite quickly) the number of distinct Smirnov words over the ordered three-letter alphabet $(\theta, \alpha, \beta)$
-- that start with the letter $\theta$, end with the letter $\theta$,  and contain `numberOfThetas' letters $\theta$,
-- `numberOfAlphas' letters $\alpha$, and `numberOfBetas' letters $\beta$".
-- Ses Remark A.1(i) of the monograph.
-- Call for instance
--    ghci> numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet (2,1,1)
-- to get the result:
--    2
-- Call
--    ghci> numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet (100,101,102)
-- to get the result:
--    201089778403474502041263056255634240679724473093327703535374720634792000
-- Call
--    ghci> numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet (100,101,103)
-- to get the result:
--    200898337258764415881665509553182189803220546519552639080371507839062000
numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet (numberOfThetas, numberOfAlphas, numberOfBetas)
  | numberOfThetas < 2 || numberOfAlphas < 1 || numberOfBetas < 1 = -1 -- "N/A: Bad Parikh vector"
  | otherwise =
    if odd (numberOfThetas + numberOfAlphas + numberOfBetas)
      then choose
             (numberOfThetas - 1)
             ((numberOfThetas + numberOfAlphas - numberOfBetas - 1) `div` 2) *
           choose
             ((numberOfThetas + numberOfAlphas + numberOfBetas - 3) `div` 2)
             (numberOfThetas - 2)
      else (numberOfThetas + numberOfAlphas - numberOfBetas) *
           choose
             (numberOfThetas - 1)
             ((numberOfThetas + numberOfAlphas - numberOfBetas) `div` 2) *
           choose
             (((numberOfThetas + numberOfAlphas + numberOfBetas) `div` 2) - 2)
             (numberOfThetas - 2)

numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet :: ParikhVector3 -> Integer
-- Name means "To get the number of distinct Smirnov words over the ordered three-letter alphabet $(\theta, \alpha, \beta)$
-- that start with the letter $\theta$, end with the letter $\beta$,  and contain `numberOfThetas' letters $\theta$,
-- `numberOfAlphas' letters $\alpha$, and `numberOfBetas' letters $\beta$".
-- Ses Remark A.1(ii) of the monograph.
-- Call for instance
--    ghci> numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet (1,1,1)
-- to get the deep result:
--    1
-- Call
--    ghci> numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet (100,101,102)
-- to get the result:
--    201089778403474502041263056255634240679724473093327703535374720634792000
-- Call
--    ghci> numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet (100,101,103)
-- to get the result:
--    200898337258764415881665509553182189803220546519552639080371507839062000
numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet (numberOfThetas, numberOfAlphas, numberOfBetas)
  | numberOfThetas < 1 || numberOfAlphas < 1 || numberOfBetas < 1 = -1 -- "N/A: Bad Parikh vector"
  | otherwise =
    if odd (numberOfThetas + numberOfAlphas + numberOfBetas)
      then (numberOfThetas + numberOfBetas - numberOfAlphas) *
           choose
             (numberOfThetas - 1)
             ((numberOfThetas + numberOfBetas - numberOfAlphas - 1) `div` 2) *
           choose
             ((numberOfThetas + numberOfAlphas + numberOfBetas - 3) `div` 2)
             (numberOfThetas - 1)
      else choose
             (numberOfThetas - 1)
             (((numberOfThetas + numberOfBetas - numberOfAlphas) `div` 2) - 1) *
           choose
             (((numberOfThetas + numberOfAlphas + numberOfBetas) `div` 2) - 1)
             (numberOfThetas - 1) +
           (numberOfThetas + numberOfBetas - numberOfAlphas) *
           choose
             (numberOfThetas - 1)
             ((numberOfThetas + numberOfBetas - numberOfAlphas) `div` 2) *
           choose
             (((numberOfThetas + numberOfAlphas + numberOfBetas) `div` 2) - 2)
             (numberOfThetas - 1)

numberOfThetaThetaSmirnovWordsOverFourLetterAlphabet :: ParikhVector4 -> Integer
-- Name means "To get (very) slowly the number of distinct Smirnov words over the ordered four-letter alphabet $(\theta, \alpha, \beta, \gamma)$
-- that start with the letter $\theta$, end with the letter $\theta$,  and contain `numberOfThetas' letters $\theta$,
-- `numberOfAlphas' letters $\alpha$, `numberOfBetas' letters $\beta$", and `numberOfGammas' letters $\gamma$".
-- Ses Remark A.2(i) of the monograph.
-- Call for instance
--    ghci> numberOfThetaThetaSmirnovWordsOverFourLetterAlphabet (2,1,1,1)
-- to get the result:
--    6
-- Call
--    ghci> numberOfThetaThetaSmirnovWordsOverFourLetterAlphabet (50,50,50,50)
-- to get (while you're drinking your cup of coffee) the result:
--    24750644668431820114189421583478394152725307383342852612238121200086886771960920383455832784
numberOfThetaThetaSmirnovWordsOverFourLetterAlphabet (numberOfThetas, numberOfAlphas, numberOfBetas, numberOfGammas)
  | numberOfThetas < 2 ||
      numberOfAlphas < 1 || numberOfBetas < 1 || numberOfGammas < 1 =
    -1 -- "N/A: Bad Parikh vector"
  | otherwise =
    sum
      [ sum
        [ doThetaThetaCalculations
          (numberOfThetas, numberOfAlphas, numberOfBetas, numberOfGammas)
          (p, r, s, t)
        | s <- [p .. maximalP]
        , t <- [r .. maximalR]
        ]
      | p <- [0 .. maximalP]
      , r <- [0 .. maximalR]
      ]
  where
    maximalP = numberOfThetas - 1
    maximalR =
      floor
        (fromIntegral
           (numberOfAlphas + numberOfBetas + numberOfGammas - numberOfThetas + 1) /
         2)

numberOfThetaAlphaSmirnovWordsOverFourLetterAlphabet :: ParikhVector4 -> Integer
-- Name means "To get (very-very-very) slowly the number of distinct Smirnov words over the ordered four-letter alphabet $(\theta, \alpha, \beta, \gamma)$
-- that start with the letter $\theta$, end with the letter $\alpha$,  and contain `numberOfThetas' letters $\theta$,
-- `numberOfAlphas' letters $\alpha$, `numberOfBetas' letters $\beta$", and `numberOfGammas' letters $\gamma$".
-- Ses Remark A.2(ii) of the monograph.
-- Call for instance
--    ghci> numberOfThetaAlphaSmirnovWordsOverFourLetterAlphabet (1,1,1,1)
-- to get the result:
--    2
-- Call
--    ghci> numberOfThetaAlphaSmirnovWordsOverFourLetterAlphabet (50,50,50,50)
-- to get (while you're drinking your cup of tea and eating your cookie) the result:
--    22374564603851832469328717862439587618212359050430002225243193765248012214699912956207299262
numberOfThetaAlphaSmirnovWordsOverFourLetterAlphabet (numberOfThetas, numberOfAlphas, numberOfBetas, numberOfGammas)
  | numberOfThetas < 1 ||
      numberOfAlphas < 1 || numberOfBetas < 1 || numberOfGammas < 1 =
    -1 -- "N/A: Bad Parikh vector"
  | otherwise =
    sum
      [ sum
        [ doThetaAlphaCalculations
          (numberOfThetas, numberOfAlphas, numberOfBetas, numberOfGammas)
          (p, r, s, t)
        | s <- [p .. maximalP]
        , t <- [r .. maximalR]
        ]
      | p <- [0 .. maximalP]
      , r <- [0 .. maximalR]
      ]
  where
    maximalP = numberOfThetas - 1
    maximalR =
      floor
        (fromIntegral
           (numberOfAlphas + numberOfBetas + numberOfGammas - numberOfThetas) /
         2)

doThetaThetaCalculations ::
     (Integer, Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
doThetaThetaCalculations (numberOfThetas, numberOfAlphas, numberOfBetas, numberOfGammas) (p, r, s, t) = do
  let expo2A =
        -numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
        2 * s -
        2 * r +
        4 * t -
        3
  let expo2B = expo2A + 2
  let expo3A =
        numberOfAlphas + numberOfBetas + numberOfGammas - 2 * numberOfThetas + s +
        r -
        3 * t +
        2
  let expo3B = expo3A - 2
  let preProductA =
        choose
          (numberOfThetas - s - 1)
          (-numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
           2 * s -
           r +
           3 * t -
           3) *
        choose
          (-numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
           2 * s +
           3 * t -
           3)
          (-numberOfAlphas + numberOfThetas + p - s + t - 1) *
        choose
          (-numberOfBetas - numberOfGammas + 2 * numberOfThetas - 2 * p + 2 * t -
           2)
          (-numberOfBetas + numberOfThetas - p + t - 1)
  let preProductB =
        choose
          (numberOfThetas - s - 1)
          (-numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
           2 * s -
           r +
           3 * t -
           1) *
        choose
          (-numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
           2 * s +
           3 * t)
          (-numberOfAlphas + numberOfThetas + p - s + t) *
        choose
          (-numberOfBetas - numberOfGammas + 2 * numberOfThetas - 2 * p + 2 * t)
          (-numberOfBetas + numberOfThetas - p + t)
  let prePreProductA =
        if expo2A >= 0
          then preProductA * (2 ^ expo2A)
          else preProductA `div` (2 ^ (-expo2A))
  let prePrePreProductA =
        if expo3A >= 0
          then prePreProductA * (3 ^ expo3A)
          else prePreProductA `div` (3 ^ (-expo3A))
  let prePreProductB =
        if expo2B >= 0
          then preProductB * (2 ^ expo2B)
          else preProductB `div` (2 ^ (-expo2B))
  let prePrePreProductB =
        if expo3B >= 0
          then prePreProductB * (3 ^ expo3B)
          else prePreProductB `div` (3 ^ (-expo3B))
  multinomialCoeff [p, r, s - p, t - r, numberOfThetas - s - 1] *
    (prePrePreProductA - prePrePreProductB)

doThetaAlphaCalculations ::
     (Integer, Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> Integer
doThetaAlphaCalculations (numberOfThetas, numberOfAlphas, numberOfBetas, numberOfGammas) (p, r, s, t) = do
  let expo2A =
        -numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
        2 * s -
        2 * r +
        4 * t -
        2
  let expo2B = expo2A + 1
  let expo2C = expo2A + 2
  let expo3A =
        numberOfAlphas + numberOfBetas + numberOfGammas - 2 * numberOfThetas + s +
        r -
        3 * t +
        1
  let expo3B = expo3A - 1
  let expo3C = expo3A - 2
  let preProductA =
        choose
          (numberOfThetas - s - 1)
          (-numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
           2 * s -
           r +
           3 * t -
           2) *
        choose
          (-numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
           2 * s +
           3 * t -
           2)
          (-numberOfAlphas + numberOfThetas + p - s + t) *
        choose
          (-numberOfBetas - numberOfGammas + 2 * numberOfThetas - 2 * p + 2 * t -
           2)
          (-numberOfBetas + numberOfThetas - p + t - 1)
  let preProductB =
        choose
          (numberOfThetas - s - 1)
          (-numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
           2 * s -
           r +
           3 * t -
           1) *
        choose
          (-numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
           2 * s +
           3 * t -
           1)
          (-numberOfAlphas + numberOfThetas + p - s + t)
  let preProductC =
        choose
          (numberOfThetas - s - 1)
          (-numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
           2 * s -
           r +
           3 * t) *
        choose
          (-numberOfAlphas - numberOfBetas - numberOfGammas + 3 * numberOfThetas -
           2 * s +
           3 * t)
          (-numberOfAlphas + numberOfThetas + p - s + t)
  let prePreProductA =
        if expo2A >= 0
          then preProductA * (2 ^ expo2A)
          else preProductA `div` (2 ^ (-expo2A))
  let prePrePreProductA =
        if expo3A >= 0
          then prePreProductA * (3 ^ expo3A)
          else prePreProductA `div` (3 ^ (-expo3A))
  let prePreProductB =
        if expo2B >= 0
          then preProductB * (2 ^ expo2B)
          else preProductB `div` (2 ^ (-expo2B))
  let prePrePreProductB =
        if expo3B >= 0
          then prePreProductB * (3 ^ expo3B)
          else prePreProductB `div` (3 ^ (-expo3B))
  let prePreProductC =
        if expo2C >= 0
          then preProductC * (2 ^ expo2C)
          else preProductC `div` (2 ^ (-expo2C))
  let prePrePreProductC =
        if expo3C >= 0
          then prePreProductC * (3 ^ expo3C)
          else prePreProductC `div` (3 ^ (-expo3C))
  multinomialCoeff [p, r, s - p, t - r, numberOfThetas - s - 1] *
    (prePrePreProductA +
     (prePrePreProductB + prePrePreProductC) *
     choose
       (-numberOfBetas - numberOfGammas + 2 * numberOfThetas - 2 * p + 2 * t)
       (-numberOfBetas + numberOfThetas - p + t))

multinomialCoeff :: [Integer] -> Integer
-- This function for calculating multinomial coefficients is borrowed from a function presented
-- in https://hackage.haskell.org/package/combinatorial-0.1.0.1/docs/src/Combinatorics.html#multinomial
multinomialCoeff = product . mapAdjacent choose . scanr1 (+)

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
-- This function is borrowed from a function presented
-- in https://hackage.haskell.org/package/utility-ht-0.0.16/docs/src/Data.List.HT.Private.html#mapAdjacent
mapAdjacent f xs = zipWith f xs (tail xs)
