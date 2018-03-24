all: build
.PHONY: all
build:
	cd rada-git && stack build
.PHONY: build
test: build
	cd rada-git && stack exec -- rada-git test
.PHONY: test
