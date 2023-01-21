module FromScratch where

data SignalFunction a b = SF (a -> (b, SignalFunction a b))

runSignalFunction _ [] = []
runSignalFunction (SF signalFunction) ls = let (b, nextSF) = signalFunction (head ls)
    in b : runSignalFunction nextSF (tail ls)

exampleSignal = SF (\x -> (not x, exampleSignal))

exampleSignal2 = SF (\x -> if x then (x, exampleSignal2) else (x, constantlyFalse)) 
    where
        constantlyFalse = SF (\_ -> (False, constantlyFalse))

compose :: SignalFunction a b -> SignalFunction b c -> SignalFunction a c
compose (SF sf1) (SF sf2) = SF (\x -> 
    let (b, newSF1) = sf1 x
        (c, newSF2) = sf2 b
    in (c, compose newSF1 newSF2) 
    )