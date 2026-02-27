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
    var ret8 struct{}
    var a__0 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    var x2 bool = a__0._0
    var x3 bool = a__0._1
    switch x2 {
    case true:
        var b__1 bool = x3
        print__T_bool(b__1)
    case false:
    }
    var c__2 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: true,
    }
    var x5 bool = c__2._0
    var x6 bool = c__2._1
    switch x5 {
    case true:
        var d__3 bool = x6
        ret8 = print__T_bool(d__3)
    case false:
        ret8 = struct{}{}
    }
    return ret8
}

func print__T_bool(value__0 bool) struct{} {
    var ret9 struct{}
    var t7 string = bool_to_string(value__0)
    ret9 = string_print(t7)
    return ret9
}

func main() {
    main0()
}
