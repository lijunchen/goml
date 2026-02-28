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
    var t__0 T
    var t3 string
    var x0 bool
    var x1 struct{}
    var t7 string
    var t9 string
    t__0 = B{
        _0: true,
        _1: struct{}{},
    }
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
    t3 = int32_to_string(1)
    string_print(t3)
    goto b1
    b3:
    x0 = t__0.(B)._0
    x1 = t__0.(B)._1
    goto b5
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
    t7 = int32_to_string(2)
    string_print(t7)
    goto b6
    b8:
    t9 = int32_to_string(3)
    string_print(t9)
    goto b6
}

func main() {
    main0()
}
