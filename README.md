# Suffix Tree Project

## Overview

This project involves the implementation of suffix trees and their associated operations using Racket, focusing on functional programming concepts. The project is divided into four stages, each building upon the previous one to enhance the functionality and efficiency of the suffix tree data structure.

## Stages

### Stage 1: Suffix Tree Construction
- Implemented the basic structure of suffix trees, including the creation of atomic and compact suffix trees.
- Focused on basic operations like inserting suffixes and navigating the tree.

### Stage 2: Suffix Tree Operations
- Expanded functionality to include various operations such as searching for patterns and identifying distinct substrings.
- Introduced concepts of recursion and higher-order functions for better data manipulation.

### Stage 3: Applications of Suffix Trees
- Implemented key applications:
  - `substring?`: Checks if a substring exists within a given text.
  - `longest-common-substring`: Identifies the longest common substring between two texts.
  - `repeated-substring-of-given-length`: Finds a substring of a specified length that appears multiple times in a text.
- Utilized variable binding techniques like `let`, `let*`, and named `let` for efficient variable management.

### Stage 4: Lazy Evaluation with Streams
- Transitioned from list-based to stream-based representations for suffix trees and suffix collections.
- Created a new `Collection` type to abstractly manage collections of suffixes, enabling flexibility in representation.
- Enhanced efficiency by employing lazy evaluation to construct suffix trees as needed, avoiding unnecessary computations.

## Key Concepts
- **Functional Programming**: Utilized concepts like recursion, higher-order functions, and lazy evaluation.
- **Abstract Data Types**: Emphasized the importance of abstraction in data handling, promoting better design and flexibility.
- **Efficiency**: Focused on optimizing performance, especially for handling long texts, by leveraging the lazy evaluation of streams.

## Conclusion
This project serves as a comprehensive exploration of suffix trees, demonstrating the application of functional programming principles in data structure design. Future work may include further optimizations and additional features for suffix tree operations.
