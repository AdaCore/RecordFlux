VSIX ?= recordflux.vsix

.PHONY: all

all: test

.PHONY: dist

dist: $(VSIX)

$(VSIX): export PATH := $(dir $(abspath Makefile))/node_modules/.bin:$(PATH)
$(VSIX): node_modules out/extension.js
	npm install @vscode/vsce && sed -i "s/\"version\": \".*\"/\"version\": \"$(shell python -m setuptools_scm | sed 's/\.dev/-dev/')\"/" package.json && vsce package -o recordflux.vsix && sed -i "s/\"version\": \".*\"/\"version\": \"0.0.0\"/" package.json

.PHONY: install

install: $(VSIX)
	code --install-extension $(VSIX)

.PHONY: build

build: node_modules out/extension.js

node_modules: package.json
	npm install

out/extension.js: src/extension.ts $(wildcard tests/*.ts)
	npm run compile

.PHONY: test

test: node_modules out/extension.js
	xvfb-run -a npm test

.PHONY: clean

clean:
	rm -rf *.vsix .vscode-test/ node_modules/ out/ package-lock.json
