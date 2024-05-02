GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

src\Omni\Par.hs : src\Omni\Par.y
	${HAPPY} ${HAPPY_OPTS} $<

src\Omni\Lex.hs : src\Omni\Lex.x
	${ALEX} ${ALEX_OPTS} $<

src\Omni\Par.y src\Omni\Lex.x : Omni.cf
	bnfc --haskell -d Omni.cf --text-token --functor --generic -o src

parser : src\Omni\Par.hs src\Omni\Lex.hs clean

clean : 
	rm -f src/Omni/Skel.hs src/Omni/ErrM.hs src/Omni/Test.hs
	rm -f src/Omni/*.bak
