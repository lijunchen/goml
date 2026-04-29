package main

import (
    _goml_fmt "fmt"
)

func uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Option__uint8 interface {
    isOption__uint8()
}

type Some struct {
    _0 uint8
}

func (_ Some) isOption__uint8() {}

type None struct {}

func (_ None) isOption__uint8() {}

type GoError = error

func main0() struct{} {
    var x__0 Option__uint8 = Some{
        _0: 42,
    }
    switch x__0.(type) {
    case Some:
        var x0 uint8 = x__0.(Some)._0
        var v__1 uint8 = x0
        var t4 string = uint8_to_string(v__1)
        println__T_string(t4)
    case None:
        println__T_string("none")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
