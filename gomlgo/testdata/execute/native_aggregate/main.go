package main

import "encoding/json"

type Person struct {
	Name string `json:"name"`
	Age int `json:"age"`
	Hidden string `json:"-"`
}

func main() {
	value := Person{Name: "Gopher", Age: 16, Hidden: "secret"}
	result, err := json.Marshal(value)
	println(err == nil, len(result), result[0], result[8], result[25])
}
