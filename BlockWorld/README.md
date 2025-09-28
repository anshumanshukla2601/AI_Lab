## AIM
To solve the **Blocks World problem** using a **depth-limited DFS** 

---

## Algorithm

1. **State Representation**  
   - Each state is represented as a `struct State`:
     - `vector<pair<char, string>> on` → stores which block is on what (other block or table)  
     - `vector<string> moves` → stores the sequence of moves to reach this state

2. **Goal Test**  
   - `isGoal(State s, vector<pair<char, string>> goal)` checks if all blocks are at their target positions.

3. **Move Generation**  
   - `getClearBlocks(State s)` finds all blocks that have nothing on top of them.  
   - `Children(State current)` generates all possible next states by:
     - Moving a clear block to the table
     - Moving a clear block onto another clear block

4. **Depth-Limited DFS**  
   - `dfs(State start, vector<pair<char, string>> goal, int depthLimit)`:
     1. Uses a stack to explore states depth-first.
     2. Checks for the goal state at each step.
     3. Explores all child states if depth limit is not exceeded.
     4. Prints the sequence of moves when the goal is reached.

---

## Time Complexity
- Worst-case complexity is **O(b^d)**, where `b` is the branching factor (number of possible moves) and `d` is the depth limit.  
- DFS may not find a solution if the depth limit is too small.  
- This is an uninformed search (no heuristic).

---
## Output
```text
Enter number of blocks: 3

Enter start state (format: Block On):
A Table
B C
C Table

Enter goal state (format: Block On):
A Table
B A
C B

Enter depth limit: 10
Goal reached!
Sequence of moves:
Move B onto A
Move C onto B
```
