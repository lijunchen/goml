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
    var inline210 int32 = 1
    var inline211 string = "x"
    var inline212 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: inline210,
        _1: inline211,
    }
    value__3 = inline212
    var t188 int32 = value__3._0
    var t189 string
    var inline208 string = _goml_runtime_core_int32_to_string(t188)
    t189 = inline208
    var t190 string = value__3._1
    var t191 string = t189 + t190
    var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
