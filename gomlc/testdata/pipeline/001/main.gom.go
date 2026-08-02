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
    var x160 bool = false
    print__T_bool(x160)
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t165)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t169 string = _goml_runtime_core_bool_to_string(self__37)
    return t169
}

func main() {
    main0()
}
