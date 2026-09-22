.PHONY: all
all: \
	docs/index.html \
	docs/Choose/index.html \
	docs/Choose/index.js \
	docs/Choose/index.css

docs/index.html: index/index.html
	mkdir -p docs
	cp index/index.html docs/index.html

docs/Choose/index.html: Choose/static/index.html
	mkdir -p docs/Choose
	cp Choose/static/index.html docs/Choose/index.html

docs/Choose/index.js: Choose/src/Main.purs Choose/spago.yaml
	mkdir -p docs/Choose
	cd Choose; spago bundle --outfile ../docs/Choose/index.js --bundle-type app

docs/Choose/index.css: Choose/static/index.css
	mkdir -p docs/Choose
	cp Choose/static/index.css docs/Choose/index.css

.PHONY: clean
clean:
	rm -rf docs