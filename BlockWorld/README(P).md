## AIM
To solve the **Blocks World problem** 

---

## Algorithm

1. **State Representation**  
   - Each state is a **list of `on(Block, Support)` terms**, e.g., `[on(a, table), on(b, a)]`.  
   - `Block` represents a block, and `Support` is either another block or `table`.

2. **Goal Test**  
   - `same_state(State1, State2)` checks if two states are equivalent by sorting their block positions.

3. **Move Generation**  
   - `make_move(State, NewState, Move)` generates possible moves:
     - Move a block to the table.
     - Move a block onto another clear block.  
   - `is_clear(Block, State)` ensures no other block is on top of the block being moved.
   - `replace_position(State, Old, New, NewState)` updates the block’s position in the state.

4. **Depth-Limited DFS**  
   - `dfs(State, Goal, Path, FinalPath, CurrentDepth, MaxDepth)` recursively explores moves:
     1. Checks if the current state equals the goal state.
     2. If depth limit is not reached, generates all valid moves.
     3. Avoids revisiting states to prevent cycles.
     4. Accumulates the sequence of moves (`Path`).

5. **Solution Printing**  
   - `print_solution(Moves)` prints moves in human-readable form:
     - `"Move A onto B"` or `"Move A to table"`.

---

## Time Complexity
- Worst-case complexity is **O(b^d)**, where `b` is the branching factor (number of possible moves) and `d` is the maximum depth limit.  
- DFS may not always find a solution if the depth limit is too small.  
- Heuristic search is not used here; this is uninformed DFS.
