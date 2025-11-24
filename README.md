# ♟️ CustomChessKell
### Play Chess by Your Own Rules

**CustomChessKell** is a customizable chess engine built in **Haskell**. It reads rules and generate them from a configuration file.

Want a Pawn that moves backwards? A Knight that jumps 4 squares? A Rook that slides diagonally? **You can define it.**

---

## 📸 Board Visualization

```text
5 | ♜ ♞ . . ♚
4 | ♟ . . . .
3 | . . . . .
2 | ♙ . . . .
1 | ♖ ♘ . . ♔
   ----------
    a b c d e
````

-----

## ✨ Features

  * **📝 Fully Customizable Rules:** Define board size, starting positions, and piece movements using a simple YAML file.
  * **🧠 Smart Move Validator:** The engine automatically calculates valid moves based on your custom definitions.
  * **🛡️ Check Safety:** Built-in logic prevents you from making illegal moves that put your King in danger.
  * **🎨 Professional CLI:** Clear, readable ASCII board using algebraic notation (e.g., `a2 a3`) and Unicode symbols.
  <!-- * **🧱 Robust Architecture:** Built using Functional Programming principles for maximum reliability. -->

-----

## 🚀 Getting Started

### Prerequisites

  * [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

### Installation & Run

1.  Clone the repository:

    ```bash
    git clone [https://github.com/your-username/customchesskell.git](https://github.com/your-username/customchesskell.git)
    cd customchesskell
    ```

2.  **Build and Run:**
    *Note: If you are on Windows, run the `chcp` command first to ensure chess symbols display correctly.*

    ```bash
    # (Windows Only) Enable UTF-8 for chess symbols
    chcp 65001

    # Build and start the game
    stack run
    ```

-----

## 🛠️ How to Create Your Own Pieces

The magic happens in `rules.yaml`. You can edit this file to invent new pieces.

### The "Language" of Movement

The engine understands three types of movement commands:

1.  **Step:** Move exactly one square (like a King).
2.  **Jump:** Leap over other pieces to a specific square (like a Knight).
3.  **Slide:** Move continuously in a direction until blocked (like a Rook or Bishop).

### Example: Creating a "Mega Knight"

```yaml
- name: MegaKnight
  symbol_white: 'M'
  symbol_black: 'm'
  moves:
    - type: Jump
      offset: [3, 0]  
```

-----

## 🧩 Technical Overview

This project demonstrates core **Functional Programming** concepts:

### 1\. The Validator

Before the game starts, the config file passes through a strict validation pipeline. We use the **`Either` Monad** to "fail-fast." If your rules contain a logic error (e.g., a slide with an invalid direction), the engine refuses to start, guaranteeing a crash-free game loop.

### 2\. The Interpreter Engine

Instead of hard-coding logic like `if (piece == Rook)`, the engine acts as an **Interpreter**. It parses the Abstract Syntax Tree (AST) of your `rules.yaml` and executes the `Step`, `Jump`, or `Slide` commands dynamically.

### 3\. Pure Functions & Immutability

The game state is immutable. To check if a move is safe (not checkmate), the engine simulates the move in a "parallel universe" (a temporary board state), checks the logic, and then discards it—all without ever mutating the actual game board.

-----

<!-- ## 🤝 Contributing

Feel free to open issues or submit pull requests if you have ideas for new movement types (e.g., "Teleport")\!

## 📄 License

Distributed under the MIT License. See `LICENSE` for more information. -->