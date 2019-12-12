import Prelude hiding (Left, Right)
data Cursor = CreateCursor {x:: Float, y:: Float, angle :: Float} deriving(Show)
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
--builderRepeat (Repeat d []) = []
builderRepeat (Repeat 0 (x:xs)) = []
builderRepeat (Repeat d (x:xs)) = megaBuilder( (x:xs) ) ++ (builderRepeat (Repeat (d-1) (x:xs)))

builder :: Instruction -> [Instruction]
builder inst = case inst of 
    Forward f -> [builderForward (Forward f)]
    Left l -> [builderLeft (Left l)]
    Right r -> [builderRight (Right r)]
    Repeat rep (x:xs) -> (builderRepeat (Repeat rep (x:xs)))

megaBuilder :: [Instruction] -> [Instruction]
--megaBuilder [] (i:xi) = megaBuilder ([(builder (head (i:xi)))]) (tail (i:xi))
megaBuilder [] = []
megaBuilder (i:xi) = builder (head (i:xi)) ++ megaBuilder (tail (i:xi))


moveCursor :: Instruction -> Cursor-> Cursor
moveCursor inst cursor = case inst of 
            Forward f -> CreateCursor (newxcoordinate cursor f) (newycoordinate cursor f) (angle cursor)
            Left l -> CreateCursor (x cursor) (y cursor) ((angle cursor)+l)
            Right r -> CreateCursor (x cursor) (y cursor) ((angle cursor)-r)
            _ -> CreateCursor (x cursor) (y cursor) (angle cursor)

newxcoordinate c n = (x c) + n*cos(angle c)
newycoordinate c n = (y c) + n*sin(angle c)

logoskell2svg :: Cursor -> String
logoskell2svg c = "x2=\""++ show(x c) ++"\" y2=\""++ show(y c) ++"\" stroke=\"red\" /> \n<line x1=\""++ show(x c) ++"\" y1=\""++ show(y c) ++"\" "

megaLogoskell :: [Instruction]-> Cursor -> String
megaLogoskell [] c = ""
megaLogoskell (i:is) c = (logoskell2svg (moveCursor (head (i:is)) c)) ++ (megaLogoskell (tail (i:is)) (moveCursor (head (i:is)) c))

gigaLOGOSKELL :: String -> String
gigaLOGOSKELL svg = if ((last svg) == '<' ) then (init svg) else gigaLOGOSKELL (init svg)


main = do 
    instruct <- getLine
    let curs = CreateCursor 100 100 0
    let listInstru = megaBuilder (read instruct :: [Instruction])
    let final = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"200\" height=\"200\">\n<title>Exemple</title>\n<line x1=\"100.000\" y1=\"100.000\" " ++ gigaLOGOSKELL (megaLogoskell listInstru curs) ++ "</svg>" 
    putStrLn final

