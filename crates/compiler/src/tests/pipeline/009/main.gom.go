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
    var t7 string
    var x0 bool
    var x1 bool
    var t11 string
    var t13 string
    var t16 string
    var t18 string
    switch t__0.(type) {
    case A:
        goto b2
    case B:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return struct{}{}
    b2:
    t7 = int32_to_string(1)
    string_print(t7)
    goto b1
    b3:
    x0 = t__0.(B)._0
    x1 = t__0.(B)._1
    switch x1 {
    case true:
        goto b5
    case false:
        goto b9
    default:
        panic("non-exhaustive match")
    }
    b4:
    goto b1
    b5:
    switch x0 {
    case true:
        goto b7
    case false:
        goto b8
    default:
        panic("non-exhaustive match")
    }
    b6:
    goto b4
    b7:
    t11 = int32_to_string(4)
    string_print(t11)
    goto b6
    b8:
    t13 = int32_to_string(3)
    string_print(t13)
    goto b6
    b9:
    switch x0 {
    case true:
        goto b11
    case false:
        goto b12
    default:
        panic("non-exhaustive match")
    }
    b10:
    goto b4
    b11:
    t16 = int32_to_string(4)
    string_print(t16)
    goto b10
    b12:
    t18 = int32_to_string(2)
    string_print(t18)
    goto b10
}

func main0() struct{} {
    var t20 T
    var t21 T
    var t22 T
    t20 = B{
        _0: true,
        _1: true,
    }
    test(t20)
    t21 = B{
        _0: false,
        _1: true,
    }
    test(t21)
    t22 = B{
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
