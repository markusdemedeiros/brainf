-- Monadic BrainFuck Interpreter

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Sequence hiding (length)

-------------------------------------------------------------------------------
-- enviornment and language

data Tape = Tape { tape :: Seq Int
                 , ptr  :: Int
                 }

-- manipulate bytes on the tape
tapeInc, tapeDec    :: Tape -> Tape
tapeInc t           = Tape (adjust (1+)  (ptr t) (tape t)) (ptr t)
tapeDec t           = Tape (adjust (-1+) (ptr t) (tape t)) (ptr t)

tapeSet :: Int -> Tape -> Tape
tapeSet i t         = Tape (adjust (\_->i) (ptr t) (tape t)) (ptr t)

-- manipulate position of the tape head
tapeRgt, tapeLft    :: Tape -> Tape
tapeRgt t           = if ((ptr t) + 1 == (length (tape t)))
                        then Tape ((tape t) |> 0)   (ptr t + 1)
                        else Tape (tape t)          (ptr t + 1)
tapeLft t           = if ((ptr t) == 0)
                        then Tape (0 <| tape t)     0
                        else Tape (tape t)          (ptr t - 1)


tapePrint           :: Tape -> IO()
tapePrint t         = putStrLn  
                        $ (++) "Tape of length " 
                        $ show . length . tape 
                        $ t

-- brainFuck language
data Cmd            = End
                    | Rgt Cmd       -- Tape right
                    | Lft Cmd       -- Tape left
                    | Inc Cmd       -- Increase byte at tape
                    | Dec Cmd       -- Decrease byte at tape
                    | Bkt Cmd Cmd   -- Bracket [Cmd1] Cmd2
                    | Prt Cmd       -- Print byte
                    | Get Cmd       -- Get byte


-------------------------------------------------------------------------------
-- monadic interpreter
--

type Eval alpha     = ReaderT Tape (StateT Int IO) alpha

runEval             :: (Eval alpha) -> IO ((alpha, Int))
runEval ev          = runStateT (runReaderT ev (Tape (singleton 0) 0)) 0

eval                :: Cmd -> Eval Tape
eval End            = do
                        showInt "DONE"
                        ask
eval (Inc c)        = do
                        showInt "+"
                        local tapeInc (eval c)
eval (Dec c)        = do
                        showInt "+"
                        local tapeDec (eval c)
eval (Lft c)        = do
                        showInt "<"
                        modify (1+)
                        local tapeLft (eval c)
eval (Rgt c)        = do
                        showInt ">"
                        modify (-1+)
                        local tapeRgt (eval c)
eval (Bkt ci co)    = do
                        showInt "["
                        (Tape t p) <- ask
                        case index t p of
                            0 -> do 
                                    showInt "]"
                                    eval co
                            _ -> do
                                    x <- eval ci
                                    local (\_ -> x) (eval (Bkt ci co))
eval (Prt c)        = do
                        showInt "O"
                        (Tape t p) <- ask
                        liftIO $ putStrLn $ "IO OUT >> " ++ show(index t p)
                        eval c

eval (Get c)        = do
                        showInt "I"
                        liftIO $ putStr $ "IO IN  << "
                        inp <- liftIO $ getLine
                        let inpc = (read inp :: Int)
                        local (tapeSet inpc) (eval c)


-- helper functions for showing intermediate steps
showInt :: String -> Eval ()
showInt s = do
            (Tape t p) <- ask
            c <- get
            liftIO $ putStrLn $ (showSystem t p c) ++ "\t\tNEXT:" ++ s


showResult :: IO (Tape, Int) -> IO ()
showResult r1 = do
                    ((Tape t p), c) <- r1
                    putStrLn $ showSystem t p c

showSystem :: Seq Int -> Int -> Int -> String
showSystem t p c = concat . toList . (intersperse " ") .          
                            (adjust (\s -> "[" ++ s ++ "]") p) . 
                            (adjust (\s -> "|" ++ s ++ "|") (p + c)) .
                            (fmap show) $ t


-- main entry point
runProgram :: Cmd -> IO()
runProgram = showResult . runEval . eval


-------------------------------------------------------------------------------
-- test program: adding two numbers
--

testAdd :: Cmd
testAdd = Inc $ Inc                             -- c0 = 2
        $ Rgt $ Inc $ Inc $ Inc $ Inc $ Inc     -- c1 = 5
        $ Bkt ( Lft $ Inc
              $ Rgt $ Dec
              $ End 
              )                                 -- c0 = 2 + 5 = 7
        $ Lft
        $ Lft $ Rgt $ Rgt $ Rgt
        $ End

runTestAdd :: IO()
runTestAdd = runProgram testAdd


-------------------------------------------------------------------------------
-- test program: IO
--

testIO :: Cmd
testIO = Get $ Inc $ Prt $ End

runTestIO :: IO()
runTestIO = runProgram testIO
