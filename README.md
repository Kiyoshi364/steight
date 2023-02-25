# Steight

A STack based programming language, with an H at the end.

# Planned Features (TODOs)

## Near Future

* [Language-Feature]: User Types (Algebraic Data Types)
  * constructors
  * destructors (like pattern matching)

## Possible Future Language Features

* Parametric/Polymorphic Types

* Dependent Types

## Far Future

Note: that it is not ordered in any relevant way

* [Implementation]: Errors types instead of Strings
  * This should give better errors messages

* [Backend]: Compile the code to other language (C, Wasm, Zig, ...)

* [CLI]: Make a proper cli program
  * run/interpreter
  * build/compile
  * repl/interactive environment
  * test: Tsoding-style testing
    * (TODO: insert some link explaining)

* [Self-Hosting]: Reimplement the compiler in Steight

* [Language-Feature]: User defined blocks
  * something like rust macros
    (rust-docs)[https://docs.rust-lang.org/reference/macros.html]
  * inspiration on LaTeX environments (`\begin` `\end`)
  * syntax idea:
    ```
      begin user-macro
        argument1 argument2 other-args
      end user-macro
    ```

# Once Possible Features

## #push/#pop

* This should help building combinators
  and saving/restoring values
  that are going to be used later

* Imagine a second stack (a helper stack)

* It's is not needed as a builtin,
  can be implemented in terms of `dip`:
  ```
    block dip ( %0 ( %0 -- %1 ) '0 -- %1 '0 ) #todo end
  ```

  So this:
  ```
    do
      #push
      doing something lalala
      #pop
    end
  ```
  can be translated to
  ```
    do
      [ doing something lalala ] dip
    end
  ```

  Also could be used as an user defined block:
  ```
    begin save
      doing something lalala
    end save
  ```
