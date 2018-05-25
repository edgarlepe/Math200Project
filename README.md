# Project 2 – Sets and Set Operations

## Setup
1. Install [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. Clone this repository
   ```bash
   ~/Some/Dir git clone https://github.com/edgarlepe/Math200Project
   ```

## Running
1. From the project root directory run:
   ```
   ~/Some/Dir/Math200Project stack build
   ~/Some/Dir/Math200Project stack exec Math200Project-exe
   ```

## Using the Application
The application is an interpreter for basic set operations. Sets can be a
combination of any of the following types:
- Integer
- Real
- Character
- String
- Boolean

Running the program will start a REPL (Read, Eval, Print Loop):
```
~/Some/Dir/Math200Project stack exec Math200Project-exe
Interpreter>
```
The interpreter supports the following operations:
- Assignment:
  ```
  Interpreter> A = {1,2,3}
  => A = {1, 2, 3}
  Interpreter> B = {'a','b','c'}
  => B = {'a', 'b', 'c'}
  Interpreter> C = {3,4,5,6}
  => C = {3, 4, 5, 6}
  ```
- Union of sets:
  ```
  Interpreter> A ∪ B
  => {1, 2, 3, 'a', 'b', 'c'}
  Interpreter> Union(A,C)
  => {1, 2, 3, 4, 5, 6}
  Interpreter> A ∪ B ∪ C
  => {1, 2, 3, 4, 5, 6, 'a', 'b', 'c'}  
  ```
- Intersection of sets:
  ```
  Interpreter> A ∩ B
  => {}
  Interpreter> A ∩ C
  => {3}
  Interpreter> Intersection(A,C)
  => {3}
  ```
- Difference of sets:
  ```
  Interpreter> A - B
  => {1, 2, 3}
  Interpreter> A - C
  => {1, 2}
  Interpreter> Difference(A,C)
  => {1, 2}
  ```
- Cartesian Product of sets: (A × B or CartesianProduct(A,B))
  ```
  Interpreter> A × B
  => {(1,'a'), (1,'b'), (1,'c'), (2,'a'), (2,'b'), (2,'c'), (3,'a'), (3,'b'), (3,'c')}
  Interpreter> CartesianProduct(A,C)
  =>{(1,3), (1,4), (1,5), (1,6), (2,3), (2,4), (2,5), (2,6), (3,3), (3,4), (3,5), (3,6)}
  ```
- Cardinality of set:
  ```
  Interpreter> |A|
  => 3
  ```
- Power Set:
  ```
  Interpreter> P(A)
  => {{1, 2, 3}, {1, 2}, {1, 3}, {1}, {2, 3}, {2}, {3}, {}}
  ```
- Cardinality:
  ```
  Interpreter> |A|
  => 3
  ```
- Adding an element to a set:
  ```
  Interpreter> Add(4,A)
  => {1, 2, 3, 4}
  Interpreter> A
  => {1, 2, 3, 4}
  ```
- Removing an element from a set:
  ```
  Interpreter> Remove('c',B)
  => {'a', 'b'}
  Interpreter> B
  => {'a', 'b'}
  ```
- Any combination of unions, intersections, differences, and cartesian
  products:
  ```
  Interpreter> A ∪ B ∪ C ∩ B - {3} × B
  => {('a','a'), ('a','b'), ('a','c'), ('b','a'), ('b','b'), ('b','c'), ('c','a'), ('c','b'), ('c','c')}
  ```
- Checking if A is a subset of B:
  ```
  Interpreter> Subset(A,B)
  => False
  Interpreter> Subset({1,2},A)
  => True
  ```
