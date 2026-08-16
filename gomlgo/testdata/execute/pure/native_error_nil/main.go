package main

import "strconv"

func main() {
	_, err := strconv.ParseFloat("not-a-number", 64)
	println(err == nil)
}
