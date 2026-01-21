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

func _goml_trait_impl_ToString_int32_to_string(self__0 int32) string {
    var ret6 string
    ret6 = int32_to_string(self__0)
    return ret6
}

func _goml_trait_impl_ToString_bool_to_string(self__1 bool) string {
    var ret7 string
    ret7 = bool_to_string(self__1)
    return ret7
}

func _goml_trait_impl_ToString__x28_int32_x2c_int32_x29__to_string(self__2 Tuple2_int32_int32) string {
    var ret8 string
    ret8 = "(?, ?)"
    return ret8
}

func main0() struct{} {
    var ret9 struct{}
    var x__3 int32 = 123
    var t3 string = _goml_trait_impl_ToString_int32_to_string(x__3)
    string_println(t3)
    var x__4 bool = true
    var t4 string = _goml_trait_impl_ToString_bool_to_string(x__4)
    string_println(t4)
    var x__5 Tuple2_int32_int32 = Tuple2_int32_int32{
        _0: 3,
        _1: 4,
    }
    var t5 string = _goml_trait_impl_ToString__x28_int32_x2c_int32_x29__to_string(x__5)
    string_println(t5)
    ret9 = struct{}{}
    return ret9
}

func main() {
    main0()
}
