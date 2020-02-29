-include Makefile.def

.PHONY: default

default: build

build:
	elm make src/Main.elm --output=elm.js

start: build
	python -m http.server
