include $(GOROOT)/src/Make.inc

TARG=avl
GOFILES=avl.go avlinc.go

all: gofmt avl

gofmt:
	gofmt -spaces -tabindent=false -tabwidth=4 -w avl.go
	gofmt -spaces -tabindent=false -tabwidth=4 -w avlinc.go

include $(GOROOT)/src/Make.cmd
