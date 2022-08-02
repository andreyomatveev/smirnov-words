module Main where

import SmirnovWordsModule
    ( numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet,
      numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet,
      numberOfThetaThetaSmirnovWordsOverFourLetterAlphabet,
      numberOfThetaAlphaSmirnovWordsOverFourLetterAlphabet,
      Alphabet3(Theta3, Alpha3, Beta3),
      Alphabet4 (Theta4, Alpha4, Beta4, Gamma4),
      numberOfSmirnovWordsOverThreeLetterAlphabet,
      numberOfSmirnovWordsOverFourLetterAlphabet 
      )

main :: IO ()
main = do
    putStrLn ("\n" ++ "First, let us count Smirnov words over the alphabet (`theta', `alpha', `beta') that start and end with the letter `theta':")
    putStrLn ("\n" ++ "numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet (2,1,1)    returns   "  ++ show (numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet (2,1,1)))
    putStrLn ("\n" ++ "numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet (100,101,102)    returns   "  ++ show (numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet (100,101,102)))
    putStrLn ("\n" ++ "numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet (100,101,103)    returns   "  ++ show (numberOfThetaThetaSmirnovWordsOverThreeLetterAlphabet (100,101,103)))
    putStrLn ("\n" ++ "And now:")
    putStrLn ("\n" ++ "numberOfSmirnovWordsOverThreeLetterAlphabet (Beta3, Beta3) (1,1,2)    returns   "  ++ show (numberOfSmirnovWordsOverThreeLetterAlphabet (Beta3, Beta3) (1,1,2)))
    putStrLn ("\n" ++ "numberOfSmirnovWordsOverThreeLetterAlphabet (Alpha3, Alpha3) (101,100,102)    returns   "  ++ show (numberOfSmirnovWordsOverThreeLetterAlphabet (Alpha3, Alpha3) (101,100,102)))
    putStrLn ("\n" ++ "numberOfSmirnovWordsOverThreeLetterAlphabet (Alpha3, Alpha3) (101,100,103)    returns   "  ++ show (numberOfSmirnovWordsOverThreeLetterAlphabet (Alpha3, Alpha3) (101,100,103)) ++ "\n\n")
    
    putStrLn ("\n" ++ "Now, let us count Smirnov words over the same alphabet (`theta', `alpha', `beta') that start with `theta' and end with `beta':")
    putStrLn ("\n" ++ "numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet (1,1,1)    returns   "  ++ show (numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet (1,1,1)))
    putStrLn ("\n" ++ "numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet (100,101,102)    returns   "  ++ show (numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet (100,101,102)))
    putStrLn ("\n" ++ "numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet (100,101,103)    returns   "  ++ show (numberOfThetaBetaSmirnovWordsOverThreeLetterAlphabet (100,101,103)))
    putStrLn ("\n" ++ "And now:")
    putStrLn ("\n" ++ "numberOfSmirnovWordsOverThreeLetterAlphabet (Beta3, Alpha3) (101,102,100)    returns   "  ++ show (numberOfSmirnovWordsOverThreeLetterAlphabet (Beta3, Alpha3) (101,102,100)))
    putStrLn ("\n" ++ "numberOfSmirnovWordsOverThreeLetterAlphabet (Beta3, Alpha3) (101,103,100)    returns   "  ++ show (numberOfSmirnovWordsOverThreeLetterAlphabet (Beta3, Alpha3) (101,103,100)) ++ "\n\n")

    putStrLn ("\n" ++ "Let us count Smirnov words over the alphabet (`theta', `alpha', `beta', `gamma') that start and end with the letter `theta':")
    putStrLn ("\n" ++ "numberOfThetaThetaSmirnovWordsOverFourLetterAlphabet (2,1,1,1)    returns   "  ++ show (numberOfThetaThetaSmirnovWordsOverFourLetterAlphabet (2,1,1,1)))
    -- putStrLn ("\n" ++ "For a coffee break:")
    -- putStrLn ("\n" ++ "numberOfThetaThetaSmirnovWordsOverFourLetterAlphabet (50,50,50,50)    returns   "  ++ show (numberOfThetaThetaSmirnovWordsOverFourLetterAlphabet (50,50,50,50))++ "\n\n")
    putStrLn ("\n" ++ "And now:")
    putStrLn ("\n" ++ "numberOfSmirnovWordsOverFourLetterAlphabet (Gamma4, Gamma4) (1,1,1,2)    returns   "  ++ show (numberOfSmirnovWordsOverFourLetterAlphabet (Gamma4, Gamma4) (1, 1, 1, 2)) ++ "\n\n")

    putStrLn ("\n" ++ "Finally, let us count Smirnov words over the same alphabet (`theta', `alpha', `beta', `gamma') that start with `theta' and end with `alpha':")
    putStrLn ("\n" ++ "numberOfThetaAlphaSmirnovWordsOverFourLetterAlphabet (1,1,1,1)    returns   "  ++ show (numberOfThetaAlphaSmirnovWordsOverFourLetterAlphabet (1,1,1,1)))
    -- putStrLn ("\n" ++ "For a (tea + cookie) break:")
    -- putStrLn ("\n" ++ "numberOfThetaAlphaSmirnovWordsOverFourLetterAlphabet (50,50,50,50)    returns   "  ++ show (numberOfThetaAlphaSmirnovWordsOverFourLetterAlphabet (50,50,50,50)))
    putStrLn ("\n" ++ "And now:")
    putStrLn ("\n" ++ "numberOfSmirnovWordsOverFourLetterAlphabet (Gamma4, Beta4) (1,1,1,1)    returns   "  ++ show (numberOfSmirnovWordsOverFourLetterAlphabet (Gamma4, Beta4) (1, 1, 1, 1)) ++ "\n\n")
