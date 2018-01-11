debug: EMFLAGS = --debug
release: EMFLAGS = --yes

SOURCES = src/*.elm
EM = elm-make $(EMFLAGS)

debug: web

release: all

docs: docs.json

web: Tagist.js index.html default.css

clean:
	@echo Cleaning javascript files and elm-stuff
	@$(RM) *.js
	@$(RM) -r elm-stuff
	@$(RM) docs.json

all: web docs

deploy: clean release
	@echo Moving javascript to deployment branch
	@git stash --all
	@git checkout gh-pages
	@rm Tagist.js
	@git stash pop
	@echo Pushing deployment
	@git add .
	@git commit -m $(subst DATE,$(shell date),"deployment on DATE")
	@git push
	@echo Restroing mast branch
	@git checkout master

Tagist.js: $(SOURCES)
	$(EM) $< --output=$@

docs.json: $(SOURCES)
	$(EM) --docs=$@