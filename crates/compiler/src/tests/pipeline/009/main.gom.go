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
    var ret26 struct{}
    switch t__0 := t__0.(type) {
    case A:
        var t17 string = int32_to_string(1)
        ret26 = string_print(t17)
    case B:
        var x0 bool = t__0._0
        var x1 bool = t__0._1
        switch x1 {
        case true:
            switch x0 {
            case true:
                var t18 string = int32_to_string(4)
                ret26 = string_print(t18)
            case false:
                var t19 string = int32_to_string(3)
                ret26 = string_print(t19)
            }
        case false:
            switch x0 {
            case true:
                var t20 string = int32_to_string(4)
                ret26 = string_print(t20)
            case false:
                var t21 string = int32_to_string(2)
                ret26 = string_print(t21)
            }
        }
    }
    return ret26
}

func main0() struct{} {
    var ret27 struct{}
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
    var t25 T = A{}
    test(t25)
    ret27 = struct{}{}
    return ret27
}

func main() {
    main0()
}
