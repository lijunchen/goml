package main

import (
    _goml_fmt "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

type Tuple2_bool_bool struct {
    _0 bool
    _1 bool
}

type GoError = error

func main0() struct{} {
    var a__0 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    var x0 bool = a__0._0
    var x1 bool = a__0._1
    switch x0 {
    case true:
        var b__1 bool = x1
        print__T_bool(b__1)
    case false:
    default:
        panic("non-exhaustive match")
    }
    var c__2 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: true,
    }
    var x3 bool = c__2._0
    var x4 bool = c__2._1
    switch x3 {
    case true:
        var d__3 bool = x4
        print__T_bool(d__3)
    case false:
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t11 string = bool_to_string(value__0)
    string_print(t11)
    return struct{}{}
}

func main() {
    main0()
}
