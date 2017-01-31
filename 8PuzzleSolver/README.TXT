README.TXT

8 TILE PROBLEM
By: Cassie Kresnye

USE OF PROGRAM
To use this program, scroll to the bottom of the page to the 'USE' section. Once there, place the desired start state in START and the desired goal in GOAL (There are some predefined start and goal states in the test area that can be used too!), then input the desired search in SEARCH. Run the program, and check the console for your answer!

ABOUT THE PROGRAM
This program solves an 8 tile game and gives the solution path (both in state form and a graphic representation). This is done through the following representation of a state: (1 2 3 4 5 6 7 8 0), where 0 signifies the space. To accomplish this, it can use depth first, breadth first, best first, or branch-n-bound (the desired search is passed into the general search). 


DEVELOPMENT JOURNAL
2/15/16 1pm ---------------------------------------------------
First day coding! I have already drawn out my models and got my algorithm down for my successor function, I just have to code it. Today, I plan to get my rotator function working. (If this doesn't work, I'll just iterate all possibilities)

Done with the pretty upper layer stuff, working on successor functions. My goal for this one is to rotate the "board" until the space is in one of 3 positions (middle middle, top middle, or top left)

Rotators are done (I can rotate clockwise and counterclockwise, wooooo!)

Got the rotators to work, and am currently working in the nested conditions in the find space function. Once I find a way to store the rotated value, i want to call a specific function on the path to add on the new possibilities, and then unrotate the list.

2/16/16 10ish pm-----------------------------------------------
Want to just get the successor mostly working tonight, then will touch bfs and dfs tomorrow morning.

TopLeft successor generator done.
TopMiddle successor generator done.
Center successor generator done.

I can generate all the new possible states. I will need to rotate those states back to their original state in the find space function tomorrow, and will also need to check in the successor method if a state created already exists.

2/17/16 9ish am--------------------------------------------------
Finish work from yesterday and get those uninformed searches done.
Alrighty, need to go tutor, so my stopping point right now is I am creating the addToPaths functions where I have to check if a state is already in the path, if it is not, then I create a new path with the new state at the end, pop that in a list, and continue until all generated states are iterated through.

-successor done, work on testbfs when i get back, it runs forever right now

Figured out the issue, if the first possible state was already used, I threw out the others. fixed now. testBFS is done, will now make a generalized search.
need to fix ss issue?

issue with bfs with ss. 

2/17/17 7ish pm---------------------------------------------------
GAHHHH so quick side rant, apparently I was using a start state that could not obtain a specific goal state. Everything works fine, I just was using the wrong dumb goal state... so basically just wasted an hour solving a problem that wasn't even really a problem.

dfs,bfs, and best first (with number of tiles out of place) all work (with the correct start and end states...)

all 4 searches are done! :D