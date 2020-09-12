module Reduction where

import PiCalculusTypes



--reduce :: Process ->

-- delete a given element from list
del :: Eq a => a -> [a] -> [a]  
del a ls = filter ((/=) a) ls

-- insert an element at given index- shift backwards
ins :: Eq a => Int -> [a] -> a -> [a]
ins i ls x = (take i ls) ++ [x] ++ (drop (i) ls)

-- delete an element with given index from list
pop :: Eq a => Int -> [a] -> [a]
pop i ls = pre ++ post 
	where
	-- take first i elements
	pre = take i ls
	-- remove first element from remaining list
	post = drop (i+1) ls

run plist =
	foldl tmp plist allpairs
	where
		n = (length plist) - 1
		allpairs = [ (i, j) | i <- [0..n], j <- [0..n] ]
		tmp ls t = runTwoInParallel ls (fst t) (snd t)

-- run two given processes in parallel and append
runAndAppend :: [Process] -> Process -> Process -> [Process]
runAndAppend plist p q =
	remainder ++ result
	where 
		remainder = del q (del p plist)
		result = runParallel p q

-- select two processes out of n parallel processes to evaluate
runTwoInParallel :: [Process] -> Int -> Int -> [Process]
runTwoInParallel plist i j =
	new
	where
		new = ins j new_ $ result !! 1
		new_ = ins i remainder $ result !! 0 
		remainder = del (plist !! j) (del (plist !! i) plist)
		result = runParallel (plist !! i) (plist !! j)

-- two processes run in parallel and "react"
runParallel :: Process -> Process -> [Process]
runParallel (Prefix c cOpt p) (Prefix c_ cOpt_ p_) =
    case pair of
    	[] -> [(Prefix c cOpt p), (Prefix c_ cOpt_ p_)]
    	ls -> let
    			channels = head ls
    			cnew = del (fst channels) c
    			cOptnew = del (fst channels) cOpt
    			cnew_ = del (snd channels) c_
    			cOptnew_ = del (snd channels) cOpt_
    		  in
    			[(Prefix cnew cOptnew p), 
    			 (Prefix cnew_ cOptnew_ p_)]
    where
    	chPairs = 
    		[ ((c1, c2), inverse c1 c2) | 
    		  c1 <- (c ++ cOpt), 
    		  c2 <- (c_ ++ cOpt_) ]
    	matching = filter (\el -> (snd el) == True) chPairs
    	pair = map fst matching


runParallel p1 p2 = error $ "Process " ++ (show p1) 
                                ++ " " ++ (show p2)
                                ++ " cannot communicate!\n"
                                ++ "Word makes no sense" 


-- check if process is in end state
finish :: Process -> Bool
finish (Lexeme a) = True
finish (Prefix [] cOpt p) = True
finish (Prefix _ cOpt p) = False  
finish (Parallel ls) =
	foldl (\val y -> val && (finish y)) True ls
