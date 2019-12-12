import Prelude hiding (Left, Right)
import System.Exit (exitSuccess)
import Debug.Trace

data Crayon = Crayon {point::(Float, Float), angle::Float}

addCrayons :: Crayon -> Crayon -> Crayon
addCrayons (Crayon p1 d1) (Crayon p2 d2) = Crayon ((fst p1)+(fst p2), (snd p1) + (snd p2)) (d1+d2)

data Instruction = Forward Float | Left Float | Right Float | Repeat Float [Instruction] deriving (Show, Read)

--function :: Instruction -> [Instruction]
--function (Forward c) = (Forward c):[]
--function (Left c) = (Left c):[]
--function (Right c) = (Right c):[]
--function (Repeat c instructions) = case c of
--    1 -> instructions
--    _ -> instructions ++ function (Repeat (c-1) instructions)

function :: [Instruction] -> [Instruction]
function i = case i of
   [] -> i
   (x:[]) -> case x of
       (Forward c) -> (Forward c):[]
       (Left c) -> (Left r):[]
           where r = c*pi/180
       (Right c) -> (Right r):[]
           where r = c*pi/180
       (Repeat c instructions) -> case c of
           1 -> function instructions
           _ -> (function instructions) ++ function ((Repeat (c-1) instructions):[])
   (x:xs) -> (function (x:[]))++function(xs)


type Program = [Instruction]

logoskell2svg :: Program -> Crayon -> String -> (Crayon, String)
logoskell2svg p c svg = case p of
   [] -> (c, svg)
   (x:[]) -> case x of
       (Forward v) -> (c2, svg ++ (convertCrayonsToSvg c c2))
           where x1 = fst (point c)
                 y1 = snd (point c)
                 a = angle c
                 x2 = x1 + (v * (cos a))
                 y2 = y1 + (v * (sin a))
                 c2 = Crayon (x2,y2) a
       Left v -> (cleft, svg)
           where cleft = Crayon (point c) ((angle c) + v)
       Right v -> (cright, svg)
           where cright = Crayon (point c) ((angle c) - v)
   (x:xs) -> logoskell2svg xs crayon_x svg_x
       where crayon_x = fst (logoskell2svg (x:[]) c svg)
             svg_x = snd (logoskell2svg (x:[]) c svg)


convertCrayonsToSvg :: Crayon -> Crayon -> String
convertCrayonsToSvg c1 c2 = "<line x1=\"" ++ x1 ++ "\" y1=\"" ++ y1 ++ "\" x2=\"" ++ x2 ++ "\" y2=\"" ++ y2 ++ "\" stroke=\"red\" />" ++ "\n"
   where x1 = show(fst (point c1))
         y1 = show(snd (point c1))
         x2 = show(fst (point c2))
         y2 = show(snd (point c2))




--splitLine :: String -> [String]
--splitLine
--
--createProgram :: [String] -> [Instruction] -> [Instruction]
--createProgram (x:xs) instr = case x of:
--                        "Forward" -> instr ++ [Forward ]


--launch :: [Instruction] -> String
--launch instr = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"200\" height=\"200\">\n<title>Exemple</title>\n"
--    ++ (logoskell2svg program crayon []) ++ "</svg>"


main = do
   putStrLn "Enter line of Logoskell code:"
   line <- getLine
   --let instr = (read line :: Instruction)
   --let program = instr:[]
   let program1 = (read line :: Program)
   let program2 = function program1
   let crayon = Crayon (100,100) 0
   --putStrLn (show instr)
   putStrLn ("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"200\" height=\"200\">\n<title>Exemple</title>\n" ++ (snd (logoskell2svg program2 crayon [])) ++ "</svg>")
   --putStrLn (snd (logoskell2svg program crayon []))
   --print "blablabla"
