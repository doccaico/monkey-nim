## monkey-nim

Monkey programming language interpreter designed in [Writing An Interpreter In Go](https://interpreterbook.com). I written this in Nim for learning purpose.

I skipped ...
- 4.4 - Array
- 4.5 - Hashes
- 4.6 - The Grand Finale

## Usage
```
# Arc
$ nim r --gc:arc src/monkey_nim.nim
# Orc
$ nim r --gc:orc src/monkey_nim.nim
```

## Test
```
$ testament r tparser.nim
$ testament r tlexer.nim
$ testament r tevaluator.nim
```
