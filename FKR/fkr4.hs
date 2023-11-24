
data Figure =
  Circle Float |
  Triangle Float Float Float |
  Square Float
  deriving Show

getFigureSurface :: Figure -> Float
getFigureSurface (Circle r) = pi * r ^ 2
getFigureSurface (Triangle a b c) =
  let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))
getFigureSurface (Square a) = a ^ 2

class Surface a where
  getSurface :: a -> Float

instance Surface Figure where
  getSurface = getFigureSurface


fig1 = Circle 4
fig2 = Triangle 1 1 1
fig3 = Square 12


main = do
  print (show(fig1) ++ " - Surface: " ++ show(getSurface(fig1)))
  print (show(fig2) ++ " - Surface: " ++ show(getSurface(fig2)))
  print (show(fig3) ++ " - Surface: " ++ show(getSurface(fig3)))

  print (show(getFigureSurface(fig1)))
  print (show(getSurface(fig1)))