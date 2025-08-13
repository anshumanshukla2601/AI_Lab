# Tic-Tac-Toe with Magic Square AI

## 📌 Overview
This is a **Tic-Tac-Toe game** in C++ where:
- The user plays as **X**
- The computer plays as **O**
- The computer uses the **magic square method** to make optimal moves

The magic square approach maps the Tic-Tac-Toe grid to a **Magic Square** (numbers 1–9 arranged so every row, column, and diagonal sums to 15).  
The AI uses this mapping to **check winning and blocking opportunities** efficiently.

---

## 🎯 Magic Square Mapping
The board is mapped as follows:

| Position | Magic Value |
|----------|-------------|
| 1        | 8           |
| 2        | 1           |
| 3        | 6           |
| 4        | 3           |
| 5        | 5           |
| 6        | 7           |
| 7        | 4           |
| 8        | 9           |
| 9        | 2           |

### Magic Square Layout:
```
8 | 1 | 6
--+---+--
3 | 5 | 7
--+---+--
4 | 9 | 2
```
Any **winning line** in Tic-Tac-Toe corresponds to **three magic numbers summing to 15**.

---

## ⚙️ AI Strategy
The computer plays as follows:
1. **Win if possible** – If AI can form a sum of 15, it plays that move.
2. **Block opponent** – If the player can win in the next move, block them.
3. **Take the center** if available.
4. **Take a corner** if available.
5. **Take a side** if available.
6. Otherwise, pick the first available space.

---

## 📜 Game Flow
1. The game alternates turns between **user** and **computer**.
2. After each move, the board is printed.
3. The game ends when:
   - The user wins
   - The computer wins
   - The board is full (**draw**)

---

## 🖥 Example Gameplay
```
Tic-Tac-Toe (Magic Square AI)
You are X, computer is O.

Board:
 1 | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

User's turn
Your move (1-9): 5

Board:
 1 | 2 | 3
---+---+---
 4 | X | 6
---+---+---
 7 | 8 | 9

Computer's turn
Computer chooses index 0 (pos 1)

Board:
 O | 2 | 3
---+---+---
 4 | X | 6
---+---+---
 7 | 8 | 9
```

---

## 🛠 Compilation & Execution
### **Compile**
```bash
g++ -o tictactoe tictactoe.cpp
```

### **Run**
```bash
./tictactoe
```

---

## 📂 File Structure
```
tictactoe.cpp   # Main game code
README.md       # Documentation (this file)
```

---

## 📌 Notes
- Input should be numbers 1–9 corresponding to positions on the board.
- Invalid moves (out of range or already filled) prompt the player to try again.
- The AI will always play optimally — it **cannot lose** unless you force a draw.

---
