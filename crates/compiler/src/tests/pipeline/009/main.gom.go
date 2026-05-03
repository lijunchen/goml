package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
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

func test(t__0 T) struct{} {
    switch t__0.(type) {
    case A:
        var t8 string = int32_to_string(1)
        println__T_string(t8)
    case B:
        var x0 bool = t__0.(B)._0
        var x1 bool = t__0.(B)._1
        switch x1 {
        case true:
            switch x0 {
            case true:
                var t12 string = int32_to_string(4)
                println__T_string(t12)
            case false:
                var t14 string = int32_to_string(3)
                println__T_string(t14)
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x0 {
            case true:
                var t17 string = int32_to_string(4)
                println__T_string(t17)
            case false:
                var t19 string = int32_to_string(2)
                println__T_string(t19)
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

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
