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

type Tuple2_bool_bool struct {
    _0 bool
    _1 bool
}

func main0() struct{} {
    var a__0 Tuple2_bool_bool
    var x0 bool
    var x1 bool
    var t4 string
    var t6 string
    var t9 string
    var t11 string
    a__0 = Tuple2_bool_bool{
        _0: true,
        _1: true,
    }
    x0 = a__0._0
    x1 = a__0._1
    switch x1 {
    case true:
        goto b2
    case false:
        goto b6
    default:
        panic("non-exhaustive match")
    }
    b1:
    return struct{}{}
    b2:
    switch x0 {
    case true:
        goto b4
    case false:
        goto b5
    default:
        panic("non-exhaustive match")
    }
    b3:
    goto b1
    b4:
    t4 = int32_to_string(789)
    string_print(t4)
    goto b3
    b5:
    t6 = int32_to_string(456)
    string_print(t6)
    goto b3
    b6:
    switch x0 {
    case true:
        goto b8
    case false:
        goto b9
    default:
        panic("non-exhaustive match")
    }
    b7:
    goto b1
    b8:
    t9 = int32_to_string(123)
    string_print(t9)
    goto b7
    b9:
    t11 = int32_to_string(789)
    string_print(t11)
    goto b7
}

func main() {
    main0()
}
