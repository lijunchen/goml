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

func main0() struct{} {
    var ret5 struct{}
    var t__0 T = B{
        _0: true,
        _1: struct{}{},
    }
    switch t__0 := t__0.(type) {
    case A:
        var t2 string = int32_to_string(1)
        ret5 = string_print(t2)
    case B:
        var x0 bool = t__0._0
        switch x0 {
        case true:
            var t3 string = int32_to_string(2)
            ret5 = string_print(t3)
        case false:
            var t4 string = int32_to_string(3)
            ret5 = string_print(t4)
        }
    }
    return ret5
}

func main() {
    main0()
}
