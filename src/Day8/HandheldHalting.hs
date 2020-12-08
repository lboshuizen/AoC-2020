module Day8.HandheldHalting (halting) where

import Utils (stoi)
import Data.Array

data Ins = Add Int
         | Jmp Int
         | Nop Int
         | Hlt
         | Trp
         deriving (Eq, Show)

decode :: (String,Int) -> Ins
decode ("nop",p) = Nop p
decode ("acc",p) = Add p
decode ("jmp",p) = Jmp p

type Cpu = (Int,Int)
type Memory = Array Int Ins

program :: [(String,Int)] -> Memory
program xs = p // [(l, Hlt)]
    where
        l = length xs
        p = listArray (0,l) . map decode $ xs

parse :: String -> (String,Int)
parse s = (mne,stoi p)
           where mne:p:_ = words s

compile = map parse

step :: Ins -> Cpu -> Cpu
step (Nop _) (ip,acc) = (ip+1,acc)
step (Add n) (ip,acc) = (ip+1,acc+n)
step (Jmp n) (ip,acc) = (ip+n,acc)
step Trp _ = error "hit trap"
step Hlt _ = error "end of program"

runTillTrap :: Cpu -> [Int] -> Memory -> (Int,[Int])
runTillTrap cpu@(pc,a) hist m 
    | ins == Trp = (a,hist)
    | otherwise = runTillTrap (step ins cpu) (pc:hist) (m // [(pc,Trp)])
    where ins = m ! pc

part1 :: [String] -> Int
part1 = fst . runTillTrap (0,0) [] . program . compile

swap :: Ins -> Ins
swap (Jmp n) = Nop n
swap (Nop n) = Jmp n

replaceable :: Ins -> Bool
replaceable (Jmp _) = True
replaceable (Nop _) = True
replaceable _ = False

rewrite :: Memory -> [Int] -> [Memory]
rewrite m h = [ m // [(i, swap ins)] | i <- h,let ins = m!i, replaceable ins ]

run :: Cpu -> Memory -> Maybe Int
run cpu@(pc,a) m
    | ins == Trp = Nothing
    | ins == Hlt = Just a
    | otherwise = run (step ins cpu) (m // [(pc,Trp)])
    where ins = m ! pc

part2 :: [String] -> Int
part2 xs = unwrap . head . dropWhile (==Nothing) . map (run (0,0)) $ rewrite p hist
    where p = program . compile $ xs
          hist = snd . runTillTrap (0,0) [] $ p
          unwrap (Just a) = a

halting :: [String] -> Int
halting = part2

