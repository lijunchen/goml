package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Person struct {
    name string
}

type GoError = error

func main0() struct{} {
    var name__0 string = "Alice"
    var p__1 Person = Person{
        name: name__0,
    }
    var t2 string = p__1.name
    string_println(t2)
    return struct{}{}
}

func main() {
    main0()
}
