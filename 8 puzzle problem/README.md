# 8-Puzzle Solver using BFS and DFS

## **Problem Statement**
The **8-Puzzle Problem** is a sliding puzzle consisting of a 3x3 grid with 8 numbered tiles (1 to 8) and one empty space (0).  
The objective is to move the tiles by sliding them into the empty space until the **goal state** is reached:

```
Goal State:
1 2 3
4 5 6
7 8 0
```

Given an **initial configuration**, determine whether it can be solved using **Depth-First Search (DFS)** and **Breadth-First Search (BFS)** algorithms.

---

## **Objective**
1. Solve the 8-puzzle problem using **DFS** and **BFS**.
2. Explore and understand **state space search**.
3. Compare DFS and BFS in terms of:
   - Time complexity
   - Space complexity
   - Path to solution (if found)

---

## **Data Structures Used**
1. **Vector of vectors (`vector<vector<int>>`)**  
   - Represents the 3x3 puzzle grid.

2. **Stack (`stack<vector<vector<int>>`)**  
   - Used in DFS to explore depth-first.

3. **Queue (`queue<vector<vector<int>>`)**  
   - Used in BFS to explore breadth-first.

4. **Set (`set<string>`)**  
   - Stores visited states to avoid cycles.

5. **Map (optional in BFS)**  
   - Can store parent-child relationships for reconstructing the path (not implemented here).

---

## **Algorithms**

### **Depth-First Search (DFS)**
- Uses a **stack** to explore states.
- Follows a **path deeply** until reaching the goal or dead end.
- May get stuck in deep branches but uses **less memory** than BFS.
  
**Pseudocode**:
```
Push initial state to stack
While stack is not empty:
    Pop top state
    If state is goal:
        Success
    Generate all possible moves (up, down, left, right)
    Push unvisited states to stack
If stack empty -> No solution
```

### **Breadth-First Search (BFS)**
- Uses a **queue** to explore states **level by level**.
- Guarantees the **shortest solution** if one exists.
- Requires **more memory** than DFS.

**Pseudocode**:
```
Enqueue initial state
Mark as visited
While queue is not empty:
    Dequeue front state
    If state is goal:
        Success
    Generate all possible moves (up, down, left, right)
    Enqueue unvisited states
If queue empty -> No solution
```

---

## **State Generation**
- Locate the **empty tile (0)**.
- Generate all **possible moves** by sliding adjacent tiles into the empty space:
  - **Up** (row-1)
  - **Down** (row+1)
  - **Left** (col-1)
  - **Right** (col+1)
- Encode each state into a **string** to store in `visited` set.

**Example**:
```
Initial State:      Move Up:
1 2 3              1 0 3
4 0 6   ---->      4 2 6
7 5 8              7 5 8
```

---

## **Utility Functions**
1. **`genMove(state)`**  
   - Generates all possible next states from the current state.

2. **`encode(state)`**  
   - Converts a 2D vector into a string key for `visited` set.

3. **`solve(initial_state)`**  
   - Performs BFS or DFS search.

---

## **Use Cases**
- **Artificial Intelligence (AI) search algorithms demonstration**.
- **Understanding uninformed search strategies**.
- **Testing performance differences between BFS and DFS**.
- **Can be extended for A* or IDA* heuristic searches**.

---

## **Sample Outputs**

### **DFS Example**
**Input State:**
```
1 2 3
4 5 6
8 0 7
```
**Output:**
```
Success
```

### **BFS Example**
**Input State:**
```
8 1 2
0 4 3
7 6 5
```
**Output:**
```
Success
```

---

## **Comparison of BFS and DFS**
| Feature           | DFS                           | BFS                            |
|-------------------|-------------------------------|--------------------------------|
| Completeness      | Not guaranteed                | Guaranteed if solution exists  |
| Optimality        | Not optimal                   | Always optimal                 |
| Memory Usage      | Low                           | High                           |
| Time Complexity   | O(b^d)                        | O(b^d)                         |

Where:  
- **b** = branching factor (max 4 for 8-puzzle)  
- **d** = depth of solution  

---

## **How to Run**
1. Compile and run the respective C++ file:
```bash
g++ dfs_8puzzle.cpp -o dfs
./dfs

g++ bfs_8puzzle.cpp -o bfs
./bfs
```

2. Modify the **`initial_state`** in `main()` to test different inputs.

---
