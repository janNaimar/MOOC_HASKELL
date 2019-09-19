speller :: [[Char]] -> [Char]
speller [] = ""
speller [word]= word ++ " is for " ++ word

speller words = let final = last words  -- words = wordsmini ++ final
                    wordsmini = init words
                    texte =  foldl (\acc x -> acc ++ [x!!0] ++" is for " ++ x ++ ", ")
                              ""
                              wordsmini


                in
                  texte ++"and "++[final!!0]++" is for " ++ final
 







