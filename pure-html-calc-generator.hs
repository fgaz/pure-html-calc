import System.Directory (createDirectoryIfMissing)
import Data.List (intersperse, intercalate)


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
  \ " ++ concatMap (mkbtn' enablenums) nums ++ " \
  \ " ++ concatMap (mkbtn' enableops) ops ++ " \
  \ " ++ mkbtn (not $ null screen) "←" ".." ++ " \
  \ " ++ mkbtn (not $ null screen) "C" (intercalate "/" $ replicate (length $ takeWhile (/= '=') screen) "..") ++ " \
  \  </form>\
  \  <p>By <a href=\"http://fgaz.github.io\">fgaz</a></p>\
  \</body>\
  \ "
  where
    btnattr enabled x = "formaction=\"" ++ x ++ "/index.html\" " ++ (if enabled then "" else "disabled")
    mkbtn enabled name dest = "<button type=\"submit\" " ++ btnattr enabled dest ++ ">" ++ name ++ "</button>"
    mkbtn' enabled x = mkbtn enabled x x

--generates the html of a result page
resulthtml actions = template False False (actions ++ "=" ++ show (calcresult actions))

--all the file-making functions
mkresultfile actions = writeFile (actions2dir actions ++ "/index.html") $ resulthtml actions
mknumselectfile actions = writeFile (actions2dir actions ++ "/index.html") $ template True False actions
mkopselectfile  actions = writeFile (actions2dir actions ++ "/index.html") $ template False True actions

calcresult str = f a b
  where
    a = read $ take 2 str --first number
    f = ch2f (str!!2) --operator
    b = read $ drop 3 str --second number

actions2dir = ("build/"++) . intersperse '/'

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
  mapM_ (mkdir . actions2dir) l4
  --then the result files
  mapM_ mkresultfile l4
  --then all the others
  mapM_ mknumselectfile l0
  mapM_ mkopselectfile l1
  mapM_ mknumselectfile l2
  mapM_ mknumselectfile l3
  
  putStrLn "Done."

