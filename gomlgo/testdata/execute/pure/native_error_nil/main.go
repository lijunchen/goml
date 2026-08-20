package main

import "strconv"

func main() {
	_, err := strconv.ParseFloat("not-a-number", 64)
	println(err == nil)
	println(err.Error() != "")
	message := err.Error
	println(message() != "")
	_, err = strconv.ParseFloat("1.5", 64)
	println(err == nil)
}
