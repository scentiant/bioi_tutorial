{-
   Michael E. Sparks, 1-2-21

   bisect.hs - Classic root bisection method.
-}

bisectRoot :: (Fractional a, Ord a) => (a -> a) -> a -> a -> a -> Maybe a
bisectRoot fct tol lo hi
  | not (lo < hi) || loSign == hiSign = Nothing
  | abs midRes < tol || abs (hiRes + loRes) < tol = Just mid
  | otherwise = let midSign = signum midRes
                in case midSign == loSign of
                     True  -> bisectRoot fct tol mid hi
                     False -> bisectRoot fct tol lo mid
  where loRes = fct lo
        hiRes = fct hi
        loSign = signum loRes
        hiSign = signum hiRes
        mid = (lo + hi) / 2
        midRes = fct mid

{-
Consider plotting in R, to observe behavior proximal to root(s):

> myCubeFct = function(x) { (x - 5) * (x + 2) * (x + 9) }
> plot(myCubeFct,xlim=range(-12:12),ylim=range(-400:400))
> abline(h=0)
-}

myCubeFct x = (x - 5) * (x + 2) * (x + 9)
myCubeFct' x = (x - 5.0003002) * (x + 2.00002) * (x + 9.4)
