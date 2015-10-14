test: elm-stuff *.elm
	./node_modules/.bin/elm-test TestRunner.elm

elm-stuff:
	elm package install -y
