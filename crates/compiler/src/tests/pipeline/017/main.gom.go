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
    return "(?, ?)"
}

func main0() struct{} {
    var x__1 int32 = 123
    var t3 string = int32_to_string(x__1)
    string_println(t3)
    var x__2 bool = true
    var t4 string = bool_to_string(x__2)
    string_println(t4)
    var x__3 Tuple2_int32_int32 = Tuple2_int32_int32{
        _0: 3,
        _1: 4,
    }
    var t5 string = _goml_trait_impl_ToString__x28_int32_x2c_int32_x29__to_string(x__3)
    string_println(t5)
    return struct{}{}
}

func main() {
    main0()
}
