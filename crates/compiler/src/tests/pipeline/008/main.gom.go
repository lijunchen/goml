package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

type T interface {
    isT()
}

type A struct {}

func (_ A) isT() {}

type B struct {
    _0 bool
    _1 struct{}
}

func (_ B) isT() {}

type GoError = error

func main0() struct{} {
    var t__0 T = B{
        _0: true,
        _1: struct{}{},
    }
    switch t__0.(type) {
    case A:
        var t4 string = int32_to_string(1)
        string_print(t4)
        return struct{}{}
    case B:
        var x0 bool = t__0.(B)._0
        switch x0 {
        case true:
            var t8 string = int32_to_string(2)
            string_print(t8)
        case false:
            var t10 string = int32_to_string(3)
            string_print(t10)
        default:
            panic("non-exhaustive match")
        }
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
