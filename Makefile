GOPATH=$(shell pwd)
target = sc

DEPS := $(fileglob *.go)
all: $(target)


$(target): $(DEPS)
	env GOPATH=$(GOPATH) go build -o bin/$(target) github.com/sonald/sc

test: $(DEPS)
	env GOPATH=$(GOPATH) go test -v github.com/sonald/sc/lexer

.PHONY: clean

clean:
	rm bin/$(target)
