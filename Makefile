debug: EM = elm-make --debug
debug: all

release: EM = elm-make --yes
release: all

clean:
	@echo Cleaning javascript files and elm-stuff
	@$(RM) *.js
	@$(RM) -r elm-stuff

all: Tagist.js index.html

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

Tagist.js: src/Tagist.elm
	$(EM) $< --output=$@

