module ReductionSpec where

import Test.Hspec
import Test.QuickCheck

import PiCalculusTypes
import Reduction

noun :: Process
noun = Prefix [Output "subj"] 
	       [Input "det"] (Lexeme "noun-joined") 

anoun :: Process
anoun = Prefix [Output "obj"] 
		   [Output "instr", Input "det"] (Lexeme "noun-joined")

verb :: Process
verb = Prefix [Input "subj", Input "obj"] 
			[Output "clause"] (Lexeme "verb-joined")

-- after subject-verb combination
noun_ :: Process
noun_ = Prefix [] [Input "det"] (Lexeme "noun-joined")

verb_ :: Process
verb_ = Prefix [Input "obj"] [Output "clause"] 
					(Lexeme "verb-joined")

-- after object-verb combination
anoun_ :: Process
anoun_ = Prefix [] [Output "instr", Input "det"] 
			(Lexeme "noun-joined")

verb__ :: Process
verb__ = Prefix [] [Output "clause"]
			(Lexeme "verb-joined")

-- add a determiner
det :: Process
det = Prefix [Output "det"] [] (Lexeme "determiner")

det_ :: Process
det_ = Prefix [] [] (Lexeme "determiner")

noun__ :: Process
noun__ = Prefix [] [] (Lexeme "noun-joined")


s_noun = "!subj . ?det . noun-joined"
s_verb = "?subj . ?obj . !clause . verb-joined"
s_noun_verb = s_noun ++ " | " ++ s_verb ++ " | "

printRun :: [Process] -> Int -> Int -> String
printRun plist i j = 
	(show plist) ++ "\n->\n" ++ (show plist_)
	where
		plist_ = runTwoInParallel plist i j

spec :: Spec 
spec = do
	describe "Process printing" $ do
		it "Prefixed process" $ do
			show noun `shouldBe` s_noun
		it "Parallel Process" $ do
			show (Parallel [noun, verb]) 
			`shouldBe` s_noun_verb

	describe "Is process finished" $ do
		it "Unfinished process" $ do
			finish noun `shouldBe` False
		it "Finished process" $ do
			finish noun_ `shouldBe` True
		it "Parallel process - unfinished" $ do
			finish (Parallel [noun_, verb_, anoun]) 
			`shouldBe` False
		it "Parallel process - finished" $ do
			finish (Parallel [noun_, noun_]) `shouldBe` True

	describe "Run two processes in parallel" $ do
		it "Two processes with matching channels" $ do
			runParallel noun verb `shouldBe` [noun_, verb_]

	describe "\n\n\nReduction: Interactive" $ do
		it ("\nSelect two processes to run in parallel\n" 
			++ (printRun [noun, anoun, verb] 0 2))
			$ do
			runTwoInParallel [noun, anoun, verb] 0 2 
			`shouldBe`
			[noun_, anoun, verb_]
		it ("\nReduces processes further\n" ++ 
			(printRun [noun_, anoun, verb] 1 2))
		    $ do
			runTwoInParallel [noun_, anoun, verb_] 1 2
			`shouldBe`
			[noun_, anoun_, verb__]
		it ("\nAdding a determiner:\n" 
			++ (printRun [noun_, anoun_, verb__, det] 0 3)) 
			$ do
			runTwoInParallel 
				[noun_, anoun_, verb__, det] 0 3
			`shouldBe`
				[noun__, anoun_, verb__, det_] 

	describe "\n\n\nWhen reduction doesn't work" $ do
		it ("\nIncoherent sentence: does not reduce further\n" ++
			(printRun [noun_, verb] 0 1))
		   $ do
		   	runTwoInParallel [noun_, verb] 0 1
		   	`shouldBe`
		   	[noun_, verb]


