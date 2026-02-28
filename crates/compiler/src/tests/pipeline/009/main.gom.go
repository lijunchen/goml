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

func test(t__0 T) struct{} {
    switch t__0.(type) {
    case A:
        var t7 string = int32_to_string(1)
        string_print(t7)
        return struct{}{}
    case B:
        var x0 bool = t__0.(B)._0
        var x1 bool = t__0.(B)._1
        switch x1 {
        case true:
            switch x0 {
            case true:
                var t11 string = int32_to_string(4)
                string_print(t11)
            case false:
                var t13 string = int32_to_string(3)
                string_print(t13)
            default:
                panic("non-exhaustive match")
            }
            return struct{}{}
        case false:
            switch x0 {
            case true:
                var t16 string = int32_to_string(4)
                string_print(t16)
            case false:
                var t18 string = int32_to_string(2)
                string_print(t18)
            default:
                panic("non-exhaustive match")
            }
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t20 T = B{
        _0: true,
        _1: true,
    }
    test(t20)
    var t21 T = B{
        _0: false,
        _1: true,
    }
    test(t21)
    var t22 T = B{
        _0: false,
        _1: false,
    }
    test(t22)
    test(A{})
    return struct{}{}
}

func main() {
    main0()
}
