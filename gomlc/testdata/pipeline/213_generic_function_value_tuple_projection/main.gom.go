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
    var inline159 int32 = 1
    var inline160 string = "x"
    var inline161 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: inline159,
        _1: inline160,
    }
    value__3 = inline161
    var t137 int32 = value__3._0
    var t138 string
    var inline157 string = _goml_runtime_core_int32_to_string(t137)
    t138 = inline157
    var t139 string = value__3._1
    var t140 string = t138 + t139
    var inline154 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t140)
    _goml_runtime_core_string_println(inline154)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
