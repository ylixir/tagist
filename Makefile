debug: EM = elm-make --debug
debug: all

release: EM = elm-make
release: all

clean:
	rm *.js

all: Tagist.js index.html

Tagist.js: src/Tagist.elm
	$(EM) $< --output=$@
