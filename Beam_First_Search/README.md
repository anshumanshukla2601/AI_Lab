# Path Finding Using Beam Search Algorithm  

## Aim  
The aim of this project is to implement the **Beam Search algorithm** to find a path from an initial position to a goal position in a 2D grid while avoiding obstacles (rivers). Beam Search is a heuristic search strategy that expands only the **best k candidates (beam width)** at each level, reducing memory usage compared to A*.  

---

## Data Structures Used  
1. **`vector<int>`** – Represents a position on the grid as `{row, col}`.  
2. **`vector<string> (beam)`** – Stores current candidates to expand, limited by beam width.  
3. **`unordered_map<string, vector<int>> (um)`** – Maps encoded positions to their coordinates.  
4. **`unordered_map<string, string> (parent)`** – Tracks parent of each state for path reconstruction.  
5. **`set<string>` (visited)** – Stores already expanded nodes.  
6. **`vector<vector<int>> (river)`** – Grid representation marking obstacles (`1` for river, `0` for free cell).  

---

## Algorithm – Beam Search  
1. **Initialize**:  
   - Start with the initial state in the beam.  
   - Set its parent to `"0"`.  
2. **Iterative Expansion**:  
   - For each state in the beam, generate children (valid moves).  
   - Compute heuristic value `h(n)` = Manhattan distance to goal.  
   - Collect children as candidate states.  
3. **Beam Selection**:  
   - Sort candidates by heuristic.  
   - Keep only the **top k (beam width)** states for the next level.  
4. **Goal Check**:  
   - If a candidate matches the goal, reconstruct the path.  
   - If no candidates remain, terminate (path not found).  

---

## State Generation  
The agent can move in **8 directions** if valid and not blocked:  
- Up `(x-1, y)`  
- Down `(x+1, y)`  
- Left `(x, y-1)`  
- Right `(x, y+1)`  
- Diagonal moves: `(x±1, y±1)`  

---

## Utility Functions  
- **`checkGoal(curr, goal)`** → Checks if current position equals the goal.  
- **`encode(curr)`** → Converts coordinates into a unique string key.  
- **`isValid(x,y,size,river)`** → Validates if a position is within bounds and not a river.  
- **`h_value(curr, goal)`** → Manhattan distance heuristic.  
- **`genMove(state, size, river)`** → Generates all valid moves.  
- **`Path(parent, goalKey, um, size, river, initial, goal)`** → Reconstructs and prints the final path.  
- **`beamSearch(initial, goal, size, river, beam_width)`** → Main Beam Search implementation.  

---

## Use Cases  
- Pathfinding in **constrained environments** where memory usage must be controlled.  
- Applications in **speech recognition, machine translation, and text generation**.  
- Demonstrating **heuristic search trade-offs** between completeness and efficiency.  
- Comparing to **A\*** and **Greedy Search**.  

---

## Time Complexity  
- Each state expansion considers at most **8 neighbors**.  
- At each level, only **k candidates** are kept.  
- If depth = d, worst-case: **O(k·d·log k)** (sorting candidates each level).  
- **Overall Complexity**: Much less than A* when `k << branching factor`.  

## Space Complexity  
- At most **k states** are stored at each level.  
- Additional maps store parents and visited states.  
- **Overall Space**: O(k·d).  

---

## Sample Output  
Input:
```text
Enter size of matrix: 5
Enter coordinates of initial position (indexed from 0): 0 0
Enter coordinates of goal position (indexed from 0): 2 3
Enter size of river: 3
Enter points of river:
Point 1: 1 1
Point 2: 2 2
Point 3: 3 3
Enter beam width (k): 3
```
Output:
```text
Path Matrix:
i * . . .
. # * . .
. . # f .
. . . # .
. . . . .

Path found:
(0,0) (0,1) (1,2) (2,3)
```


---

## Advantages of Beam Search  
- Uses **less memory** than A* by limiting candidates.  
- Faster in practice for large spaces with a good heuristic.  
- Easy to implement and adapt to different problems.  

## Limitations  
- **Incomplete**: May fail to find a path if `beam_width` is too small.  
- Not guaranteed to find the **optimal path**.  
- Strongly dependent on heuristic quality and beam width selection.  
