-- Calculates the width/height of the outer square
squareSize :: Int -> Int
squareSize number = ceiling (sqrt (fromIntegral number))

-- Calculates the length of a direct line from the center to the layer containing the given number
squareDiam :: Int -> Int
squareDiam number = floor ((fromIntegral (squareSize number )) / 2)

-- Calculates the moment needed accross the outer layer to get to the closest position that has a direct line to the center
outerMovement :: Int -> Int
outerMovement number = do
   let size = squareSize number
   let diam = squareDiam number
   let clockwise = mod (number - diam - 1) size
   let counterClockwise = mod (-1 * number + diam + 1) size
   min clockwise counterClockwise

main = do
   input <- getLine
   let number = read input :: Int
   let diam = squareDiam number
   let movement = outerMovement number
   putStrLn (show (diam + movement))
