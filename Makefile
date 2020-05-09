.POSIX:
STACK = stack
LINTER = hlint
FORMATTER = stylish-haskell -i -r

.PHONY: all build lint format clean

all: build

build:
	$(STACK) build

lint:
	$(LINTER) src

format:
	$(FORMATTER) src

clean:
	$(STACK) clean
