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
    var ret7 struct{}
    var a__0 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    var x0 bool = a__0._0
    var x1 bool = a__0._1
    switch x0 {
    case true:
        var b__1 bool = x1
        var t5 string = bool_to_string(b__1)
        string_print(t5)
    case false:
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
        var t6 string = bool_to_string(d__3)
        ret7 = string_print(t6)
    case false:
        ret7 = struct{}{}
    }
    return ret7
}

func main() {
    main0()
}
