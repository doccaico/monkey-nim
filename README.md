## monkey.nim

Monkey programming language interpreter designed in [Writing An Interpreter In Go](https://interpreterbook.com). I written this in Nim (2.2.10) for learning purpose.

I skipped ...
- 4.4 - Array
- 4.5 - Hashes
- 4.6 - The Grand Finale

## Build and Run (Debug)
```
$ nim r src\monkey.nim

# or debugging (-d:trace)

$ nim r -d:trace src\monkey.nim
```

## Build (Release)
```
$ nim c -d:release src\monkey.nim
```

## Test
```
# Do them one by one
$ testament run tests\all\tparser.nim
$ testament run tests\all\tlexer.nim
$ testament run tests\all\tevaluator.nim

# or

$ testament pattern tests\all\*.nim
```
