package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
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
    var c__2 Tuple2_bool_bool
    var x3 bool
    var x4 bool
    var d__3 bool
    var b__1 bool
    a__0 = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    x0 = a__0._0
    x1 = a__0._1
    switch x0 {
    case true:
        goto b5
    case false:
        goto b6
    default:
        panic("non-exhaustive match")
    }
    b1:
    c__2 = Tuple2_bool_bool{
        _0: true,
        _1: true,
    }
    x3 = c__2._0
    x4 = c__2._1
    switch x3 {
    case true:
        goto b3
    case false:
        goto b4
    default:
        panic("non-exhaustive match")
    }
    b2:
    return struct{}{}
    b3:
    d__3 = x4
    print__T_bool(d__3)
    goto b2
    b4:
    goto b2
    b5:
    b__1 = x1
    print__T_bool(b__1)
    goto b1
    b6:
    goto b1
}

func print__T_bool(value__0 bool) struct{} {
    var t9 string
    var t10 struct{}
    t9 = bool_to_string(value__0)
    t10 = string_print(t9)
    return t10
}

func main() {
    main0()
}
