-- Inf2d Assignment 1 2018-2019
-- Matriculation number:s1891340
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases. 
badNodesList = [(1,2),(2,2),(3,2),(4,2),(5,2),(6,2)]

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth=35
-- Why did you choose this number?
-- Because there are only 36 states in total, consider the worst situation, the robot has explored all the states, then the depth should be 35 (start from 0)


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

targetNode :: Node -> [Node]
targetNode node = [(fst node + 1, snd node), (fst node, snd node + 1), (fst node - 1, snd node), (fst node, snd node - 1)]

removeInvalid :: [Node] -> [Node]
removeInvalid nodes = [node | node <- nodes, elem node badNodesList == False, fst node >= 1, snd node >=1, fst node <= 6, snd node <=6]

next branch = [new:branch | new <- removeInvalid (targetNode (head branch)), elem new branch == False  ]


-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = (fst destination == fst curNode) && (snd destination == snd curNode)


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.
breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch destination next [] exploredList= Nothing
breadthFirstSearch destination next branches exploredList
	| checkArrival destination (head (head branches)) = Just $ (head branches)
    | elem (head (head branches)) exploredList = breadthFirstSearch destination next (drop 1 branches) exploredList
    | otherwise  = breadthFirstSearch destination next (branches++(next (head branches))) ((head (head branches)):exploredList)
    

-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next [] exploredList = Nothing
depthFirstSearch destination next branches exploredList
	| checkArrival destination (head (head branches)) = Just $ (head branches)
    | elem (head (head branches)) exploredList = depthFirstSearch destination next (drop 1 branches) exploredList
    | otherwise  = depthFirstSearch destination next ((next (head branches))++branches) ((head (head branches)):exploredList)

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch destination next [] depth = Nothing
depthLimitedSearch destination next (branch:branches) depth
    | checkArrival destination (head branch) = Just $ branch
    | depth == (length branch) - 1 = depthLimitedSearch destination next branches depth
	| elem destination badNodesList = Nothing
    | otherwise  = depthLimitedSearch destination next ((next branch)++branches) depth

-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
	| d > maxDepth = Nothing
	| depthLimitedSearch destination next [[initialNode]] d /= Nothing = depthLimitedSearch destination next [[initialNode]] d
	| otherwise = iterDeepSearch destination next initialNode (d+1)

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan position destination = (abs(fst position - fst destination))+(abs(snd position - snd destination))

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.
-- sort::Branch-> Branch
-- sort branches
-- 	| heuristic (head head(branches)) == 

sortBranch::(Node-> Int)-> Branch-> Branch-> Ordering
sortBranch heuristic branch1 branch2
	| heuristic (head branch1) > heuristic (head branch2) = GT
	| heuristic (head branch1) < heuristic (head branch2) = LT
	| otherwise = EQ

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic branches exploredList
	| checkArrival destination (head (head branches)) = Just $ (head branches)
	| elem (head (head branches)) exploredList = bestFirstSearch destination next heuristic (drop 1 branches) exploredList
	| otherwise = bestFirstSearch destination next heuristic newBranch ((head (head branches)):exploredList)
	where
	newBranch = sortBy (sortBranch (manhattan destination)) ((next (head branches)) ++ branches)


-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.
sortBranch_aStar::(Node->Int) -> (Branch-> Int)-> Branch -> Branch -> Ordering
sortBranch_aStar heuristic cost branch1 branch2
	| (heuristic (head branch1)) + (cost branch1) > (heuristic (head branch2)) + (cost branch2) = GT
	| (heuristic (head branch1)) + (cost branch1) < (heuristic (head branch2)) + (cost branch2) = LT
	| otherwise = EQ


aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost branches exploredList
	| checkArrival destination (head (head branches)) = Just $ (head branches)
	| elem (head (head branches)) exploredList = aStarSearch destination next heuristic cost (drop 1 branches) exploredList
	| otherwise = aStarSearch destination next heuristic cost newBranch ((head (head branches)):exploredList)
	where
	newBranch = sortBy (sortBranch_aStar (manhattan destination) cost) ((next (head branches)) ++ branches)
    
	
-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = length(branch) - 1


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches. 



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state. 
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game = if (terminal game) && (checkWin game 1) then 1
				else if  (terminal game) && (checkWin game 0) then -1
					else 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state. 

minimax:: Game->Player->Int
minimax game player
	| terminal game = eval game
	| player == 1 = maximum [n | thegame <- (moves game player), n <- [(minimax thegame (switch player))]]
	| player == 0 = minimum [n | thegame <- (moves game player), n <- [(minimax thegame (switch player))]]

-- | The alpha beta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state. 

max_value:: Game-> Int-> Int-> Int
max_value game a b
	| terminal game = eval game
	| otherwise = max_loop (moves game 1) a b a

max_loop:: [Game]-> Int-> Int-> Int-> Int
max_loop [] a b v = v
max_loop (game:games) a b v
	| (max v (min_value game a b)) >= b = (max v (min_value game a b))
	| otherwise = max_loop games (max a v) b (max v (min_value game a b))

min_value:: Game-> Int-> Int-> Int
min_value game a b
	| terminal game = eval game
	| otherwise = min_loop (moves game 0) a b b

min_loop:: [Game]-> Int-> Int-> Int-> Int
min_loop [] a b v = v
min_loop (game:games) a b v
	| (min v (max_value game a b)) <= a = (min v (max_value game a b))
	| otherwise = min_loop games a (min b v) (min v (max_value game a b))

alphabeta:: Game->Player->Int
alphabeta game player
	| terminal game = eval game
	| player == 1 = max_value game (-99) 99
	| player == 0 = min_value game (-99) 99

    

-- | Section 5.2 Wild Tic Tac Toe





-- | The evalWild function should be used to get the value of a terminal state. 
-- It should return 1 if either of the move types is in the correct winning position. 
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game =if (terminal game) && (checkWin game 1) then 1
				else if  (terminal game) && (checkWin game 0) then 1
					else 0


-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward. 
-- If the min player sent the game into a terminal state you should give -1 reward. 

wildmax_value:: Game-> Int-> Int-> Int
wildmax_value game a b
	| terminal game = - evalWild game
	| otherwise = wildmax_loop (movesWild game 1) a b a

wildmax_loop:: [Game]-> Int-> Int-> Int-> Int
wildmax_loop [] a b v = v
wildmax_loop (game:games) a b v
	| terminal game = evalWild game
	| (max v (wildmin_value game a b)) >= b = (max v (wildmin_value game a b))
	| otherwise = wildmax_loop games (max a v) b (max v (wildmin_value game a b))

wildmin_value:: Game-> Int-> Int-> Int
wildmin_value game a b
	| terminal game = evalWild game
	| otherwise = wildmin_loop (movesWild game 0) a b b

wildmin_loop:: [Game]-> Int-> Int-> Int-> Int
wildmin_loop [] a b v= v
wildmin_loop (game:games) a b v
	| terminal game = - evalWild game
	| (min v (wildmax_value game a b)) <= a = (min v (wildmax_value game a b))
	| otherwise = wildmin_loop games a (min b v) (min v (wildmax_value game a b))

alphabetaWild:: Game->Player->Int
alphabetaWild game player
	| player == 1 = wildmax_value game (-99) 99
	| player == 0 = wildmin_value game (-99) 99
	
-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.

		
-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning). 
-- The evalWild function should be used to get the value of a terminal state. 

minimaxWild:: Game->Player->Int
minimaxWild game player =undefined
	

			
-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
 


