import System.Directory (createDirectoryIfMissing)
import Data.List (intersperse)


ch2f '+' = (+)
ch2f '-' = (-)
ch2f '*' = (*)
ch2f ':' = (/)
ch2f _ = error "You have to define the functions associated to additional operators!"

mkdir = createDirectoryIfMissing True

--possible digits/operators. strings because of the frequent use of ++
nums = map show [0..9]
ops = ["+","-","*",":"]

--monadic functions to add the next digit/operator
plusx x str = map (str++) x
plusnums = plusx nums
plusops = plusx ops

--generates the html
template :: Bool -> Bool -> String -> String
template enablenums enableops screen = "\
  \<!DOCTYPE html>\
  \<html>\
  \<head>\
  \  <title>Pure HTML Calculator</title>\
  \  <meta charset=utf-8>\
  \</head>\
  \<body>\
  \  <h1>Pure HTML Calculator</h1>\
  \  <h2><pre>[HTMLcalc]&gt; " ++ screen ++ "</pre></h2>\
  \  <form>\
  \ " ++ (concat $ map (mkbtn' enablenums) nums) ++ " \
  \ " ++ (concat $ map (mkbtn' enableops) ops) ++ " \
  \ " ++ mkbtn ((length screen) /= 0) "←" ".." ++ " \
  \ " ++ mkbtn ((length screen) /= 0) "C" (concat $ intersperse "/" $ replicate (length $ takeWhile (/= '=') screen) "..") ++ " \
  \  </form>\
  \  <p>By <a href=\"http://fgaz.github.io\">fgaz</a></p>\
  \</body>\
  \ "
  where
    btnattr enabled x = "formaction=\"" ++ x ++ "/index.html\" " ++ (if enabled then "" else "disabled")
    mkbtn enabled name dest = "<button type=\"submit\" " ++ (btnattr enabled dest) ++ ">" ++ name ++ "</button>"
    mkbtn' enabled x = mkbtn enabled x x

--generates the html of a result page
resulthtml actions = template False False $ (actions ++ "=" ++ (show $ calcresult actions))

--all the file-making functions
mkresultfile actions = writeFile ((actions2dir actions) ++ "/index.html") $ resulthtml actions
mknumselectfile actions = writeFile ((actions2dir actions) ++ "/index.html") $ template True False actions
mkopselectfile  actions = writeFile ((actions2dir actions) ++ "/index.html") $ template False True actions

calcresult str = f a b
  where
    a = read ((str!!0):(str!!1):[]) --first number
    b = read ((str!!3):(str!!4):[]) --second number
    f = ch2f (str!!2) --operator

actions2dir = ("build/"++) . (intersperse '/')

main :: IO ()
main = do
  putStrLn "Generating the pure html calculator in build/ ..."
  
  mkdir "build"
  --start page
  writeFile "build/index.html" $ template True False ""
  
  --the 5 layers of the calculator
  let l0 = nums
  let l1 = l0 >>= plusnums
  let l2 = l1 >>= plusops
  let l3 = l2 >>= plusnums
  let l4 = l3 >>= plusnums
  
  --first all the folders
  sequence $ map (mkdir . actions2dir) l4
  --then the result files
  sequence $ map mkresultfile l4
  --then all the others
  sequence $ map mknumselectfile l0
  sequence $ map mkopselectfile l1
  sequence $ map mknumselectfile l2
  sequence $ map mknumselectfile l3
  
  putStrLn "Done."

  return ()
