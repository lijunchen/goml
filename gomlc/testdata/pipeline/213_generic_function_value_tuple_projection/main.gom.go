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
    var inline178 int32 = 1
    var inline179 string = "x"
    var inline180 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: inline178,
        _1: inline179,
    }
    value__3 = inline180
    var t156 int32 = value__3._0
    var t157 string
    var inline176 string = _goml_runtime_core_int32_to_string(t156)
    t157 = inline176
    var t158 string = value__3._1
    var t159 string = t157 + t158
    var inline173 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
    _goml_runtime_core_string_println(inline173)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
