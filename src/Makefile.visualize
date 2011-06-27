include $(GOROOT)/src/Make.inc

TARG=visualize
GOFILES=visualize.go

all: gofmt visualize

gofmt:
	gofmt -spaces -tabindent=false -tabwidth=4 -w visualize.go 

include $(GOROOT)/src/Make.cmd
