package main

import "os"

func main() {
	info, err := os.Stat("goml.toml")
	println(err == nil, info.Mode().IsRegular())
}
