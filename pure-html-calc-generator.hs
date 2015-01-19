import System.Directory (createDirectoryIfMissing)
import Data.List (intersperse)


ch2f '+' = (+)
ch2f '-' = (-)
ch2f '*' = (*)
ch2f ':' = (/)
ch2f _ = error "You have to define the functions associated to additional operators!"

mkdir = createDirectoryIfMissing True

nums = map show [0..9]
plusx x str = map (str++) x
plusnums = plusx nums
plusops = plusx ops
ops = ["+","-","*",":"]

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

mkresulthtml actions = template False False $ (actions ++ "=" ++ (show $ calcresult actions))

mkresultfile actions = writeFile ((actions2dir actions) ++ "/index.html") $ mkresulthtml actions

mknumselectfile actions = writeFile ((actions2dir actions) ++ "/index.html") $ template True False actions

mkopselectfile  actions = writeFile ((actions2dir actions) ++ "/index.html") $ template False True actions

calcresult str = f a b
  where
    a = read ((str!!0):(str!!1):[])
    b = read ((str!!3):(str!!4):[])
    f = ch2f (str!!2)

actions2dir = ("build/"++) . (intersperse '/')

main :: IO ()
main = do
  mkdir "build"
  writeFile "build/index.html" $ template True False ""
  --let l = nums >>= plusnums >>= plusops >>= plusnums >>= plusnums
  let l0 = nums
  let l1 = l0 >>= plusnums
  let l2 = l1 >>= plusops
  let l3 = l2 >>= plusnums
  let l4 = l3 >>= plusnums
  
  sequence $ map (mkdir . actions2dir) l4
  sequence $ map mkresultfile l4
  
  sequence $ map mknumselectfile l0
  sequence $ map mkopselectfile l1
  sequence $ map mknumselectfile l2
  sequence $ map mknumselectfile l3
  
  return ()
