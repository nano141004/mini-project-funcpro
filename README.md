# ♟️ CustomChessKell

### *Play Chess by Your Own Rules — Powered by Haskell*

CustomChessKell is a **fully customizable chess engine** built in **pure Haskell**.
Unlike traditional chess games with coded-in piece logic, this engine acts as an **Interpreter**: it reads piece movement behaviours from a **YAML configuration** and executes them dynamically.

Want a Pawn that moves backward?
A Knight that jumps 4 squares?
A Rook that slides diagonally forever?

You can define it — no source code changes required.

---

# 📸 Board Visualization (Example)

```
5 | ♜ ♞ . . ♚
4 | ♟ . . . .
3 | . . . . .
2 | ♙ . . . .
1 | ♖ ♘ . . ♔
    ----------
     a b c d e
```

---

# ✨ Features

### 📝 Fully Customizable Rules

Define **board size**, **starting positions**, and **piece movements** using a simple YAML file.

### 🧠 Smart Move Validator

The engine interprets your movement rules using an internal **AST (Abstract Syntax Tree)** and computes legal moves dynamically.

### 🛡️ Check Safety

Uses simulation + immutability to reject moves that put your own King in danger.

### 🎨 Clean CLI

Readable ASCII board using algebraic notation (e.g., `a2 a3`) and Unicode piece symbols.

### 🤖 Automatic Game Termination

Detects **Checkmate** and **Stalemate** using lazy evaluation for efficiency.

---

# 🚀 Getting Started

## 📦 Prerequisites

* [Haskell Stack](https://docs.haskellstack.org/)

---

## 🛠 Installation & Run

Clone the repository:

```bash
git clone https://github.com/your-username/customchesskell.git
cd customchesskell
```

### For Windows' Users — Enable UTF-8 for Unicode Chess Symbols

```bash
chcp 65001
```

### Build and Run

```bash
stack run
```

### Input the rules to be interpreted

```bash
--- CustomChessKell ---
Enter rules file path (e.g., rules.yaml): 
```

---


# 🧠 Creating Your Own Pieces

All customization happens in **`.yaml`** file (You need to make it first, or you can see the template rules - in `rules_w4.yaml`). This file controls the physics of your chess universe. The engine strictly enforces rules to prevent game-breaking logic (like infinite loops or missing kings).

The file is divided into three main sections:

---

## 1. `board_size`
Defines the dimensions of the grid.

```yaml
board_size:
  rows: 8
  cols: 8
```

  * **Note:** You can create rectangular boards (e.g., 5x8).


## 2\. `pieces`

This is where you define the "DNA" of your pieces: their names, symbols, and how they move.

### The Anatomy of a Piece

```yaml
- name: SuperPawn
  symbol_white: 'P'  # Displayed for White
  symbol_black: 'p'  # Displayed for Black
  moves: [...]       # List of movement rules
```

  * **Constraint (Uniqueness):** Every symbol defined in the file must be unique. You cannot have a Pawn and a King both using 'P'.

### The Movement DSL (Domain Specific Language)

The engine understands three specific movement types. Your piece's behavior is a list of these rules.

#### A. `Step` (Short Range)

Moves exactly **one square** to an adjacent tile.

  * **Logic:** Can move to empty squares or capture enemies. Cannot jump.
  * **Validation Constraint:** The offset must be exactly 1 square away (e.g., `[1,0]` or `[1,1]`). `[2,0]` is invalid.

<!-- end list -->

```yaml
- type: Step
  offset: [1, 0]  # Move 1 square Forward
```

#### B. `Jump` (Teleportation)

Leaps directly to a target square, ignoring any pieces in between.

  * **Logic:** Typically for Knights. Can jump over walls/pieces.
  * **Validation Constraint:** The offset must be **greater than 1 square** away. A jump of `[1,0]` is invalid (use Step instead).

<!-- end list -->

```yaml
- type: Jump
  offset: [2, 1]  # The classic "L" shape
```

#### C. `Slide` (Long Range)

Moves continuously in a specific direction until it hits the edge of the board or another piece.

  * **Logic:** Typically for Rooks/Bishops. If it hits an enemy, it can capture.
  * **Validation Constraint:** The `direction` must be a **unit vector** (length of 1).
      * Valid: `[1, 0]` (Forward), `[1, 1]` (Diagonal).
      * Invalid: `[2, 0]` (You cannot "skip" squares while sliding).

<!-- end list -->

```yaml
- type: Slide
  direction: [1, 1] # Slide diagonally North-East
```

-----

## 3\. `formation`

Defines where pieces sit at the start of the game.

**Important:** You only define the formation for **White** (the bottom side). The engine automatically mirrors this setup for **Black** on the top side.

```yaml
formation:
  - piece: SuperPawn
    position: [1, 0]  # Row 1, Col 0
  - piece: SimpleKing
    position: [0, 4]
```

### Critical Constraints (The "Gauntlet")

The engine will **refuse to start** if your formation breaks these rules:

1.  **The "Half-Board" Rule:** White pieces must be placed in the bottom half of the board (Rows `< total_rows / 2`). This ensures White and Black don't spawn on top of each other during reflection.
2.  **The King Rule:** You **must** define exactly one piece named `King` in your piece list, and you **must** place exactly one `King` in the formation. (Without a King, checkmate logic is impossible).
3.  **Collision Rule:** Two pieces cannot start on the same square.

<!-- end list -->

---

# 🧩 Technical Overview — Functional Programming Concepts

CustomChessKell was built as a showcase of **Functional Programming architecture**.
Below are the main design principles used:

---

## 1️⃣ The Gauntlet: Monadic Validation

### 🧩 Concept: Fail-Fast with the `Either` Monad

Before the game starts, your configuration file is processed by a sequential validation pipeline:

* missing pieces
* invalid symbols
* conflicting placements
* missing kings

Using `Either` allows:

✔ clean error propagation
✔ early termination on failure
✔ dependent checks (e.g., king existence → king position → king safety)

The engine refuses to run if the configuration is invalid.

---

## 2️⃣ Interpreter Pattern via ADTs

### 🧩 Concept: Data-Driven Logic

Movement rules are defined as an **AST**:

```haskell
data MoveRule
  = Step Position
  | Jump Position
  | Slide Position
```

The core engine **pattern-matches** on this AST in `getValidMoves`, meaning:

* no `if piece == ...` hacks
* infinite extensibility
* new movement rules added purely through YAML

This is a true Interpreter architecture.

---

## 3️⃣ Simulation Through Immutability

### 🧩 Concept: Snapshot Instead of Undo

Checking move safety requires simulating the board *after* a move.

Imperative engines must do:

```
apply move → check → undo move
```

Risk: mutation leaks, incorrect undo, corrupted state.

Functional approach:

```
newBoard = applyMove oldBoard
check newBoard
```

Because `oldBoard` is immutable, **simulation is safe by construction**.

---

## 4️⃣ Lazy Evaluation for Optimization

### 🧩 Concept: Efficiency Without Manual Control Flow

To detect Checkmate or Stalemate, the engine must answer:

> “Does the current player have **ANY** legal moves?”

Using:

```haskell
any hasMoves myPieces
```

Haskell evaluates `hasMoves` lazily:

* stops on the **first** valid move
* avoids computing safe moves for all pieces
* no manual `break` or `return` needed
* expensive simulations are skipped automatically

This results in optimal behavior from purely declarative code.

---

