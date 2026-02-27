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

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Tuple2_int32_int32 struct {
    _0 int32
    _1 int32
}

func _goml_trait_impl_ToString__x28_int32_x2c_int32_x29__to_string(self__0 Tuple2_int32_int32) string {
    var ret10 string
    ret10 = "(?, ?)"
    return ret10
}

func main0() struct{} {
    var ret11 struct{}
    var x__1 int32 = 123
    var t7 string = int32_to_string(x__1)
    string_println(t7)
    var x__2 bool = true
    var t8 string = bool_to_string(x__2)
    string_println(t8)
    var x__3 Tuple2_int32_int32 = Tuple2_int32_int32{
        _0: 3,
        _1: 4,
    }
    var t9 string = _goml_trait_impl_ToString__x28_int32_x2c_int32_x29__to_string(x__3)
    string_println(t9)
    ret11 = struct{}{}
    return ret11
}

func main() {
    main0()
}
