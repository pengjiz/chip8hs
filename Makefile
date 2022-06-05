.POSIX:
STACK = stack
LINTER = hlint
FORMATTER = stylish-haskell -i -r

.PHONY: all build test lint format clean

all: build

build:
	$(STACK) build

test:
	$(STACK) test

lint:
	$(LINTER) src app test

format:
	$(FORMATTER) src app test

clean:
	$(STACK) clean
