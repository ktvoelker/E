
E: a programming language
=========================

E is a programming language. I will write more about it later.

For now, you can consult the lexer and parser.  They seem to be working, at
least for simple inputs.

To use the lexer:

1. Run `ghci`
2. Run `:load Lexer` in ghci
3. Run `tokenize "" "some tokens"` in ghci

To use the parser:

1. Run `ghci`
2. Run `:load Parser` in ghci
3. Run `parseFile "" "def x = 3;"` in ghci

