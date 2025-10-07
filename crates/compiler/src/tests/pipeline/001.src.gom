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
    var ret9 struct{}
    var t7 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    var a__0 Tuple3_bool_bool_Tuple2_bool_bool = Tuple3_bool_bool_Tuple2_bool_bool{
        _0: true,
        _1: false,
        _2: t7,
    }
    var x3 Tuple2_bool_bool = a__0._2
    var x5 bool = x3._1
    var w__4 bool = x5
    var b__5 bool = w__4
    var t8 string = bool_to_string(b__5)
    string_print(t8)
    ret9 = struct{}{}
    return ret9
}

func main() {
    main0()
}
