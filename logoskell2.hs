import Prelude hiding (Left, Right)

data Instruction = Forward Float
           |Left Float
       |Right Float
   |Repeat Int [Instruction]
   deriving (Show, Read)

--type ListInstruction = [Instruction]
--ListInstruction = (read instruc :: [Instruction])

builderForward :: Instruction -> Instruction
builderForward (Forward a) = (Forward a) 

builderLeft :: Instruction -> Instruction
builderLeft (Left b) = (Left (b*pi/180))

builderRight :: Instruction -> Instruction
builderRight (Right c) = (Right (c*pi/180))

builderRepeat :: Instruction -> [Instruction]
builderRepeat (Repeat d []) = []
builderRepeat (Repeat 1 (x:xs)) = (x:xs)
builderRepeat (Repeat d (x:xs)) = (builderRepeat (Repeat (d-1) (x:xs))) ++ builder x

builder :: Instruction -> [Instruction]
builder inst = case inst of 
   Forward f -> [builderForward (Forward f)]
   Left l -> [builderLeft (Left l)]
   Right r -> [builderRight (Right r)]
   Repeat rep (x:xs) -> (builderRepeat (Repeat rep (x:xs)))

megaBuilder :: [Instruction] -> [Instruction]
--megaBuilder [] (i:xi) = megaBuilder ([(builder (head (i:xi)))]) (tail (i:xi))
megaBuilder [] = []
megaBuilder (i:xi) = (megaBuilder (tail (i:xi)) ++ (builder (head (i:xi))))

data Cursor = CreateCursor {x:: Float, y:: Float, angle :: Float} deriving(Show)

moveCursor :: Instruction -> Cursor-> Cursor
moveCursor inst cursor = case inst of 
           Forward f -> CreateCursor (newxcoordinate cursor f) (newycoordinate cursor f) (angle cursor)
           Left l -> CreateCursor (x cursor) (y cursor) ((angle cursor)+l)
           Right r -> CreateCursor (x cursor) (y cursor) ((angle cursor)-r)

newxcoordinate :: Cursor -> Float -> Float
newxcoordinate c n = (x c) + n*cos(angle c)
newycoordinate :: Cursor -> Float -> Float
newycoordinate c n = (y c) + n*sin(angle c)

logoskell2svg :: Cursor -> String
logoskell2svg c = "x2="++ show(x c) ++" y2="++ show(y c) ++" stroke=\"red\" /> \n <line x1="++ show(x c) ++" y1="++ show(y c) ++" "

megaLogoskell :: [Instruction]-> Cursor -> String
megaLogoskell [] _ = ""
megaLogoskell (i:is) c = (logoskell2svg (moveCursor (head (i:is)) c)) ++ (megaLogoskell (tail (i:is)) (moveCursor (head (i:is)) c))


main = do 
   -- putStrLn "Please enter a logoskell program: "
   instruct <- getLine
   let curs = CreateCursor 100 100 0
   let listInstru = megaBuilder (read instruct :: [Instruction])
   let final = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"200\" height=\"200\">\n<title>Exemple</title>\n" ++ (take ((length (megaLogoskell listInstru curs))- 31) (megaLogoskell listInstru curs)) ++ "</svg>" 
   putStrLn final


{--
obtenirDescription :: Instruction -> String
obtenirDescription (Forward x) ="Deplace le crayon de " ++ (show x) ++ " unites"
obtenirDescription (Left x) ="Tourne le crayon de " ++ (show x) ++ " degrés vers la gauche"
obtenirDescription (Right x) ="Tourne le crayon de " ++ (show x) ++ " degrés vers la droite"
--}
