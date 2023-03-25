import CodeWorld
import Prelude 
import Data.Text (pack)

--f x = pictures [translated i j (solidCircle 0.5) | i<-[1..3], j<-[1..3] ]

ball t = rotated (min t (rotationTime)) (translated (t/slowFactor) x ( circle 0.5)) &
          pictures [rotated ((min (t'/density) rotationTime))
                    (translated ((t'/density)/slowFactor) x (solidCircle 0.1))
                    |t'<-[0..t*density]]
      where
        density=200
        rotationTime=pi*3
        x = 0 -- if t<rotationTime then 0 else 5*sin t
        slowFactor = log (t+1)

scene :: Double -> Picture
scene t = ball t

main :: IO ()
main = animationOf scene

{-
tree :: Double -> Picture
tree t =
  translated 0 2 (
    colored brown (
      solidRectangle 0.5 4 &
      translated 0 1.75 (rotated (pi/4 - offset) (translated 0 1 (solidRectangle 0.25 2))) &
      translated 0 1.75 (rotated (-(pi/4 + offset)) (translated 0 1 (solidRectangle 0.25 2)))
      ) &
    translated offset 3 (colored green (solidCircle 2))
    )
  where offset = if t < (3/2 * pi) then 0.3 * sin (2 * t) else 0

-- Do not change the stuff below here!

scene :: Double -> Picture
scene t =
  tree t &
  countTime t

main :: IO ()
main = animationOf scene

countTime :: Double -> Picture
countTime t = scaled 0.5 0.5 (translated 5 0 (lettering (pack ("t = " ++ truncatedTime t))))

truncatedTime :: Double -> String
truncatedTime t =
  let (n,f) = properFraction t
  in show (n :: Int) ++ take 3 (tail (show f))
-}