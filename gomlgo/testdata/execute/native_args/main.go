package main

import "os"

func main() {
	println(len(os.Args), os.Args[1], os.Args[2])
}
