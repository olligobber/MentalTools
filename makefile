.PHONY: all
all: docs/index.html docs/Choose/index.html docs/Choose/index.js

docs/index.html: index/index.html
	mkdir -p docs
	cp index/index.html docs/index.html

docs/Choose/index.html: Choose/index.html
	mkdir -p docs/Choose
	cp Choose/index.html docs/Choose/index.html

docs/Choose/index.js: Choose/src/Main.purs Choose/spago.yaml
	mkdir -p docs/Choose
	cd Choose; spago bundle --outfile ../docs/Choose/index.js --bundle-type app

.PHONY: clean
clean:
	rm -rf docs