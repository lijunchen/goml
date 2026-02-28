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

type Tuple3_bool_bool_Tuple2_bool_bool struct {
    _0 bool
    _1 bool
    _2 Tuple2_bool_bool
}

func main0() struct{} {
    var t7 Tuple2_bool_bool
    var a__0 Tuple3_bool_bool_Tuple2_bool_bool
    var x3 Tuple2_bool_bool
    var x5 bool
    var w__4 bool
    var b__5 bool
    t7 = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    a__0 = Tuple3_bool_bool_Tuple2_bool_bool{
        _0: true,
        _1: false,
        _2: t7,
    }
    x3 = a__0._2
    x5 = x3._1
    w__4 = x5
    b__5 = w__4
    print__T_bool(b__5)
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t8 string
    var t9 struct{}
    t8 = bool_to_string(value__0)
    t9 = string_print(t8)
    return t9
}

func main() {
    main0()
}
