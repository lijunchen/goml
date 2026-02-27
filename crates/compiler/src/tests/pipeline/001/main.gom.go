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
    var ret15 struct{}
    var t13 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    var a__0 Tuple3_bool_bool_Tuple2_bool_bool = Tuple3_bool_bool_Tuple2_bool_bool{
        _0: true,
        _1: false,
        _2: t13,
    }
    var x10 Tuple2_bool_bool = a__0._2
    var x12 bool = x10._1
    var w__4 bool = x12
    var b__5 bool = w__4
    print__T_bool(b__5)
    ret15 = struct{}{}
    return ret15
}

func print__T_bool(value__0 bool) struct{} {
    var ret16 struct{}
    var t14 string = bool_to_string(value__0)
    ret16 = string_print(t14)
    return ret16
}

func main() {
    main0()
}
