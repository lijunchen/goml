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
    var retv4 string
    retv4 = "(?, ?)"
    return retv4
}

func main0() struct{} {
    var x__1 int32 = 123
    var t6 string = int32_to_string(x__1)
    string_println(t6)
    var x__2 bool = true
    var t7 string = bool_to_string(x__2)
    string_println(t7)
    var x__3 Tuple2_int32_int32 = Tuple2_int32_int32{
        _0: 3,
        _1: 4,
    }
    var t8 string = _goml_trait_impl_ToString__x28_int32_x2c_int32_x29__to_string(x__3)
    string_println(t8)
    return struct{}{}
}

func main() {
    main0()
}
