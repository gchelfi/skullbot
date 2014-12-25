skull : Main.hs Bot.hs Message.hs Skull.hs
	ghc --make -O2 -o $@ $^

clean :
	rm -f *.{hi,o}

% : %.hs
	ghc --make -O2 $<
