{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Ease where


easeInOutQuad :: Double -> Double -> Double -> Double -> Double
easeInOutQuad start change duration currentTime =
  let t = currentTime / (duration / 2)
      t' = t - 1
  in if t < 1
     then change / 2 * t * t + start
     else (negate change) / 2 * (t' * (t' - 2) - 1) + start

{-
t = currentTime
b = start
c = change
d = duration
Math.easeInOutQuad = function (t, b, c, d) {
        t /= d/2;
        if (t < 1) return c/2*t*t + b;
        t--;
        return -c/2 * (t*(t-2) - 1) + b;
};

-}

easeInOutExpo :: Double -> Double -> Double -> Double -> Double
easeInOutExpo start change duration currentTime
    | currentTime == 0        = start
    | currentTime == duration = start + change
    | otherwise =
          let t = currentTime / (duration / 2)
          in if t < 1
             then change / 2 * (2 ** (10 * (t - 1))) + start
             else change / 2 * ((negate (2 ** (-10 * (t - 1))) + 2)) + start
