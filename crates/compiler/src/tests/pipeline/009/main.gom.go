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
    _1 bool
}

func (_ B) isT() {}

type GoError = error

func test(t__0 T) struct{} {
    switch t__0.(type) {
    case A:
        var t8 string = int32_to_string(1)
        string_print(t8)
    case B:
        var x0 bool = t__0.(B)._0
        var x1 bool = t__0.(B)._1
        switch x1 {
        case true:
            switch x0 {
            case true:
                var t12 string = int32_to_string(4)
                string_print(t12)
            case false:
                var t14 string = int32_to_string(3)
                string_print(t14)
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x0 {
            case true:
                var t17 string = int32_to_string(4)
                string_print(t17)
            case false:
                var t19 string = int32_to_string(2)
                string_print(t19)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t22 T = B{
        _0: true,
        _1: true,
    }
    test(t22)
    var t23 T = B{
        _0: false,
        _1: true,
    }
    test(t23)
    var t24 T = B{
        _0: false,
        _1: false,
    }
    test(t24)
    test(A{})
    return struct{}{}
}

func main() {
    main0()
}
