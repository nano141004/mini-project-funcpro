# ♟️ CustomChessKell

### *Play Chess by Your Own Rules — Powered by Haskell*

CustomChessKell is a **fully customizable chess engine** built in **pure Haskell**.
Unlike traditional chess games with coded-in piece logic, this engine acts as an **Interpreter**: it reads piece movements from a **YAML configuration** and executes them dynamically.

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

### Windows Only — Enable UTF-8 for Unicode Chess Symbols

```bash
chcp 65001
```

### Build and Run

```bash
stack run
```

---

# 🧠 Creating Your Own Pieces

All customization happens in **`rules.yaml`**.

## Movement "Language" (DSL)

The engine understands three primitive commands:

### `Step`

Move exactly one square (e.g., King movement).

### `Jump`

Leap to a specific square, ignoring blockers (e.g., Knight movement).

### `Slide`

Move continuously in a direction until blocked (e.g., Rook/Bishop movement).

## Example: A “Mega Knight”

```yaml
- name: MegaKnight
  symbol_white: 'M'
  symbol_black: 'm'
  moves:
    - type: Jump
      offset: [3, 0]   # A super-long Knight-like jump
```

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

