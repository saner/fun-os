all: compiler-exec parser-exec

compiler-exec: Compiler.hs CompilerExec.hs
	ghc --make  Compiler.hs CompilerExec.hs

compiler: Compiler.hs
	ghc --make Compiler.hs

parser-exec: Parser.hs ParserExec.hs
	ghc --make ParserExec.hs

parser: Parser.hs
	ghc --make Parser.hs

clean:
	-rm -f *.hi *.o *~
