all: Main

Lexer.hs : lexer/Lexer.x
	alex lexer/Lexer.x

Parser.hs : parser/Parser.y
	happy parser/Parser.y
    
Main : Lexer.hs Parser.hs Main.hs
	ghc --make Main -i./compiler:./parser:./lexer:./typeChecker
    
clean:
	rm -f Main parser/Parser.hs lexer/Lexer.hs *.o *.hi output.hs
	rm -f */*.o */*.hi
    
