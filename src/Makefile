all: Calc

Tokens.hs : lexer/Tokens.x
	alex lexer/Tokens.x

Grammar.hs : parser/Grammar.y
	happy parser/Grammar.y
    
Calc : Tokens.hs Grammar.hs Calc.hs
	ghc --make Calc -i./compiler:./parser:./lexer
    
clean:
	rm -f Calc parser/Grammar.hs lexer/Tokens.hs *.o *.hi output.hs
	rm -f */*.o */*.hi
    
