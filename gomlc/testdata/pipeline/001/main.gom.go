package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_print(s string) struct{} {
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
    var x182 bool = false
    var inline193 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x182)
    _goml_runtime_core_string_print(inline193)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t191 string = _goml_runtime_core_bool_to_string(self__66)
    return t191
}

func main() {
    main0()
}
