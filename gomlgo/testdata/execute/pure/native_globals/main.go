package main

import (
	"io"
	"os"
)

func main() {
	_ = io.ErrUnexpectedEOF
	_ = os.Stderr
}
