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

### Critical Constraints

The engine will **refuse to start** if your formation breaks these rules:

1.  **The "Half-Board" Rule:** White pieces must be placed in the bottom half of the board (Rows `< total_rows / 2`). This ensures White and Black don't spawn on top of each other during reflection.
2.  **The King Rule:** You **must** define exactly one piece named `King` in your piece list, and you **must** place exactly one `King` in the formation. (Without a King, checkmate logic is impossible).
3.  **Collision Rule:** Two pieces cannot start on the same square.

<!-- end list -->

---

# 🧠 Technical Overview — FP Implementation & Trade-offs

This section explains the concrete design choices made in the implementation and why they matter. 

### 1. Monadic Validation 

**What it is (implementation):**
Before the engine starts, the YAML rules are parsed into `RuleSet` and passed through a validation pipeline implemented with the `Either String` monad:

```haskell
validateRuleSet :: RuleSet -> Either String RuleSet
validateRuleSet rs = do
  validatePieceNames rs
  validateKingPresence rs
  validateFormationConstraints rs
  return rs
```

**Why:**
Using `Either` keeps the validator pure and composable. Each validation step is a pure function; any `Left` short-circuits the pipeline and prevents the engine from starting with an invalid ruleset.

**What we got:**

* Simple, readable validation flow.
* No exceptions or runtime errors; invalid configs are detected early and reported.

**Limitations:**

* `Either` *short-circuits* at the first error. We get the first failing reason, not a list of all problems. For large rule files this can be irritating when debugging.
* If we want accumulated error reports (all errors at once), an `Applicative` validation (e.g., `Validation` from `validation` package) would be more appropriate.

---

### 2. Interpreter Pattern via ADTs (Move DSL)

**What it is (implementation):**
Movement rules are represented as an ADT (the DSL), parsed from YAML via `FromJSON` instances, and interpreted centrally:

```haskell
data MoveRule = Step Position | Jump Position | Slide Position

evalMove :: MoveRule -> ...
evalMove rule = case rule of
  Step off -> evalStep off
  Jump off -> evalJump off
  Slide dir -> evalSlide dir
```

**Why:**
Modeling moves as data (not behavior embedded in code) makes the engine data-driven: new movement behaviors are added by editing YAML + ADT + interpreter branch — no scattering of special-case logic.

**What we got:**

* Extensibility: new rules are additions to the language, not brittle patches.
* The compiler helps: non-exhaustive pattern matches are caught at compile time.

**Limitations:**

* The interpreter must be extended in the code when new AST constructors are added (the interpreter is the single place to change, which is purposeful but still a code change).
* Very complex behaviors (stateful piece abilities or contextful rules) may require richer DSL constructs (conditions, multi-step macros). It might be better to consider separating pure rule *syntax* from *semantics* further (e.g., small evaluator combinators) if later want user-defined rule macros or conditional rules.

---

### 3. Simulation via Immutability (Safety Check)

**What it is (implementation):**
To decide whether a candidate move is legal, the engine constructs a new board state (using `Map` operations) and checks king safety on that simulated board:

```haskell
tempBoard = Map.insert toPos piece (Map.delete fromPos board)
not (isKingInCheck rules size tempBoard player)
```

**Why:**
Immutability makes simulation trivial and safe, there is no need to mutate and then undo state. The original board is never corrupted even when simulations fail or throw.

**What we got:**

* Correctness: no rollback bugs or leftover side effects from simulations.
* Easier testing: pure functions operating on `Board` values are easy to test in isolation.

**Limitations:**

* Pure snapshots can be slower than carefully optimized in-place mutation for very high volumes of simulations.

**Future development:**

* If performance becomes an issue, profile hotspots and memoization or incremental board updates (e.g., Zobrist hashing / move application that reuses structure).

---

### 4. Lazy Evaluation for Optimization (Checkmate / Stalemate)

**What it is (implementation):**
The `hasLegalMoves` function relies on `any` and lazy evaluation to short-circuit search:

```haskell
hasLegalMoves :: RuleMap -> BoardSize -> Board -> Color -> Bool
hasLegalMoves rules size board player =
  let
    myPieces = filter (\(_, p) -> pColor p == player) (Map.toList board)
    
    hasMoves (pos, _) = not (null (getSafeMoves rules size board pos))
  in
    any hasMoves myPieces
```

**Why:**
`getSafeMoves` can be expensive (simulate + validate moves). Haskell’s lazy semantics ensure `any` stops as soon as one piece has a legal move, so many expensive computations are skipped implicitly.

**What we got:**

* The performance of an early-exit loop without manual `break` logic.
* Cleaner, declarative code that still performs well in practice.

**Limitations / caveats:**

* Laziness can mask performance problems if thunks accumulate.
* Dependence on evaluation order.

---

### 5. Type-Driven Design (Make illegal states unrepresentable)

**What it is (implementation):**
Core domain values are properly typed (e.g., `Color = White | Black`, `Position = Pos Int Int`) and raw strings/numbers are parsed once at the I/O boundary (`FromJSON` instances).

**Why:**
By converting untrusted text into strongly typed values at the boundary, the engine’s core functions operate on valid, well-formed data only. This reduces runtime error paths.

**What we got:**

* Fewer runtime checks inside the engine.
* Safer, clearer APIs between modules.

**Limitations:**

* Parser errors are located at the boundary and must be handled properly.
* The `FromJSON` logic must be maintained if the DSL extends.

**Future developments:**
Add property tests (QuickCheck) that generate random valid rulesets and assert invariants (e.g., no overlapping formation squares, exactly one King).

---

