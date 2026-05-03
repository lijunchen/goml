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

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

type Tuple3_4bool_4bool_18Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
    _2 Tuple2_4bool_4bool
}

func main0() struct{} {
    var t8 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var a__0 Tuple3_4bool_4bool_18Tuple2_4bool_4bool = Tuple3_4bool_4bool_18Tuple2_4bool_4bool{
        _0: true,
        _1: false,
        _2: t8,
    }
    var x3 Tuple2_4bool_4bool = a__0._2
    var x5 bool = x3._1
    var w__4 bool = x5
    var b__5 bool = w__4
    print__T_bool(b__5)
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t10 string = bool_to_string(value__0)
    string_print(t10)
    return struct{}{}
}

func main() {
    main0()
}
