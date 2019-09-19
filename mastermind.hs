-- afficher
-- presenter
-- demander coups
-- jouer
-- analyser 
-- let jeux = [[x,y,z,t] | x <- "CMYRGB." ,y <- "CMYRGB.", z <-"CMYRGB.", t <- "CMYRGB."]
-- length [x| (x,y) <- zip "RGM." "GBM.", x == y]
-- [x | x <-"RGG.", y <-"GGB.", x==y]
--

import System.Random
import Data.List(delete)


jeux_possibles :: [String]
jeux_possibles = [[x,y,z,t] | x <- combi, y <- combi , z <- combi , t <- combi]
  where combi = "CMYRGB."

alea_code :: IO String
alea_code = do
  seed <- getStdGen
  let (val, _) = randomR (0, (length jeux_possibles)-1) seed
  return $ jeux_possibles !! val 
  

vie :: Int
vie = 4

tailleGrille :: Int
tailleGrille = 4

secret_combi :: String
secret_combi = "RRGC"

analyser_old:: String -> String -> String
analyser_old prop secret  =
  let goodPosition =  length [x | (x,y) <- zip prop secret, x == y]
      ofusc_prop = zipWith (\ x y -> if x == y then 'X' else x) prop secret -- en une ligne ?
      ofusc_secret = zipWith (\ x y -> if x == y then 'Z' else y) prop secret
      badPosition = length [ x | x <- ofusc_prop , y <- ofusc_secret , x == y]
  in
    (replicate goodPosition '#')++(replicate badPosition '+')
      ++(replicate (4-badPosition-goodPosition) '-')


analyser:: String -> String -> String
analyser prop secret  =
  let goodPosition =  length [x | (x,y) <- zip prop secret, x == y]
      ofusc_prop = zipWith (\ x y -> if x == y then 'X' else x) prop secret -- en une ligne ?
      ofusc_secret = zipWith (\ x y -> if x == y then 'Z' else y) prop secret
      automat code secr compt = if code == ""
                              then compt
                              else if (head code) `elem` secr
                                    then
                                      automat (tail code) (delete (head code) secr) (compt + 1)
                                    else
                                      automat (tail code) secr compt
      badPosition = automat ofusc_prop ofusc_secret 0
      

  in
    (replicate goodPosition '#') ++ (replicate badPosition '+')
      ++ (replicate (tailleGrille-goodPosition-badPosition) '-')





presenter :: IO ()
presenter = do
  putStrLn ""
  putStrLn "*****************"
  putStrLn "*MasterMind Game*"
  putStrLn "*****************"
  putStrLn ""
  putStrLn "Choice : R, G, B, C, M, Y, . (red, green, blue, cyan, magenta, yellow, hole)"
  putStrLn "Combinaison , color item and hole and 4 positions."

tour :: Int -> String -> IO()
tour n secret= 
  let loop n gagne = if gagne 
                     then message_gagne n
                     else if n == 0 then message_perdu secret 
                          else do
                            comb <- demander n
                            if not  (valide comb) then loop n False
                            else let res = analyser comb secret
                                 in 
                                  if res == "####" then
                                    loop n True
                                  else do
                                    afficher_analyse res
                                    loop (n-1) False 
   in loop vie False  


valide :: String -> Bool
valide combi = combi `elem` jeux_possibles

message_gagne :: Int -> IO()
message_gagne n = putStrLn $ "You win, congratulation! ("++show n ++ " life(s) )"

message_perdu :: String -> IO()
message_perdu code = putStrLn $ "The code was " ++ code ++ ", you lose!"

demander :: Int -> IO String
demander n = do
  putStrLn $ "Enter a combinaison:        life: " ++ replicate n '*'
  getLine

jouer :: IO()
jouer = do
  presenter
  tour vie secret_combi

afficher_analyse :: String -> IO()
afficher_analyse code = putStrLn $ "Analyse: " ++ code


main :: IO()
main = jouer
