# Functional-Images

## Overview

"Functional Images" is a Haskell project aimed at implementing an embedded domain-specific language (EDSL) for working with images. The project emphasizes the application of functional programming mechanisms, type systems (including polymorphism), and lazy evaluation.

## Project Structure

The project is divided into three main stages:

1. **Stage 1: Shallow Embeddings**
   - Implement shallow embeddings for regions represented as functions.

2. **Stage 2: Deep Embeddings**
   - Explore deep embeddings for regions and transformations.
   - Focus on implementing functions that interpret abstract syntax trees (ASTs) for regions and transformations.
   - Goals:
     - Recover concrete representations.
     - Linearize nested transformation sequences.
     - Merge consecutive transformations of the same type.
     - Optimize transformations within an AST.

3. **Stage 3: Reduction Mechanism**
   - Instantiate various classes and implement a reduction mechanism for ASTs of transformations and regions.
   - Write functions to:
     - Represent ASTs as strings.
     - Construct regions using arithmetic operators.
     - Apply functions over fields of types.
     - Reduce ASTs.
     - Define compositional operations in terms of reductions.
