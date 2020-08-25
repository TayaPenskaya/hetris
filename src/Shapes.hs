-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing, isJust)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

newtype Shape = Shape [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (Shape rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]
    
    showSquare Nothing = '.'
    showSquare (Just Black) = '#' 
    showSquare (Just Grey)  = 'g' 
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [Shape (makeSquares s) | s <- shapes] 
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes = 
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

-- ** A01

-- | Returns empty row of given size
emptyRow :: Int -> Row
emptyRow size = replicate size Nothing

-- | Returns empty shape of given size
emptyShape :: (Int,Int) -> Shape
emptyShape (rowSize, rowsNum) = Shape $ replicate rowsNum $ emptyRow rowSize

-- ** A02

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize shape = (width, height) 
  where height = length rlist
        width = length xs
        rlist@(xs:_) = rows shape

-- ** A03

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount shape = length $ filter isNothing xs
  where xs = concat $ rows shape

-- * The Shape invariant

-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape shape
  | w < 1 || h < 1 || not (isRectangle $ rows shape) = False
  | otherwise = True
  where (w, h) = shapeSize shape
        
        isRectangle :: [Row] -> Bool
        isRectangle [x] = True
        isRectangle (x:xs:xss)
          | length x == length xs = isRectangle (xs:xss)
          | otherwise = False

-- * Test data generators

-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]

instance Arbitrary Colour where
  arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees clockwise
rotateShape :: Shape -> Shape
rotateShape (Shape rows) = Shape $ transpose $ reverse rows

-- ** A08

-- | shiftRight adds empty squares the left of the shape
shiftRight :: Int -> Shape -> Shape
shiftRight n (Shape rlist)
  | n <= 0 = Shape rlist
  | otherwise = shiftRight (n-1) $ Shape $ map (Nothing:) rlist

-- | shiftDown adds empty squares above of the shape
shiftDown :: Int -> Shape -> Shape
shiftDown n (Shape rlist)
  | n <= 0 = Shape rlist
  | otherwise = shiftDown (n-1) $ Shape $ emptyRow w : rlist
  where (w, h) = shapeSize $ Shape rlist
 
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (r, d) = shiftRight r . shiftDown d

-- ** A09
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape (h, v) shape = rotateShape . rotateShape $ shiftShape (h, v) $ rotateShape . rotateShape $ shape

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (w, h) shape = padShape (w - w', h - h') shape
  where (w', h') = shapeSize shape

-- * Comparing and combining shapes

-- ** A11

-- | Test if two rows overlap
rowsOverlap :: Row -> Row -> Bool
rowsOverlap r1 r2 = or $ zipWith (\x y -> isJust x && isJust y) r1 r2

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = or $ zipWith rowsOverlap (rows s1) (rows s2)

-- ** A12
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f (Shape r1) (Shape r2) = Shape $ zipWith (zipWith f) r1 r2


blackClashes :: Shape -> Shape -> Shape
blackClashes = zipShapeWith clash  
  where clash :: Square -> Square -> Square 
        clash Nothing Nothing = Nothing
        clash Nothing s       = s
        clash s       Nothing = s
        clash (Just c1) (Just c2) = Just Black

-- ** A13
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = blackClashes s1' s2'
  where 
    (s1', s2') = (padShapeTo (w, h) s1, padShapeTo (w, h) s2)
    (w1, h1) = shapeSize s1
    (w2, h2) = shapeSize s2
    (w, h) = (max w1 w2, max h1 h2)
