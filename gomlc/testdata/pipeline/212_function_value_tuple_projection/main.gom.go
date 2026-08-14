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
    var inline205 int32 = 1
    var inline206 string = "x"
    var inline207 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: inline205,
        _1: inline206,
    }
    value__3 = inline207
    var t186 int32 = value__3._0
    var t187 string
    var inline203 string = _goml_runtime_core_int32_to_string(t186)
    t187 = inline203
    var t188 string = value__3._1
    var t189 string = t187 + t188
    var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline200)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
