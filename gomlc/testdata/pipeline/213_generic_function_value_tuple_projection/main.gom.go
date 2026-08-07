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
    var inline195 int32 = 1
    var inline196 string = "x"
    var inline197 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: inline195,
        _1: inline196,
    }
    value__3 = inline197
    var t173 int32 = value__3._0
    var t174 string
    var inline193 string = _goml_runtime_core_int32_to_string(t173)
    t174 = inline193
    var t175 string = value__3._1
    var t176 string = t174 + t175
    var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
    _goml_runtime_core_string_println(inline190)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
