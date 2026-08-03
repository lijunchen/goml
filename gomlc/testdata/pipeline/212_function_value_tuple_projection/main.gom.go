package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_5int32_6string struct {
    _0 int32
    _1 string
}

func main0() struct{} {
    var value__3 Tuple2_5int32_6string
    var inline200 int32 = 1
    var inline201 string = "x"
    var inline202 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: inline200,
        _1: inline201,
    }
    value__3 = inline202
    var t181 int32 = value__3._0
    var t182 string
    var inline198 string = _goml_runtime_core_int32_to_string(t181)
    t182 = inline198
    var t183 string = value__3._1
    var t184 string = t182 + t183
    var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline195)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
