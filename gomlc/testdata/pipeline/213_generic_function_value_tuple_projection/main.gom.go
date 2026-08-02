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
    var make__2 func(int32, string) Tuple2_5int32_6string = pair__First_int32__Second_string
    var value__3 Tuple2_5int32_6string = make__2(1, "x")
    var t156 int32 = value__3._0
    var t157 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t156)
    var t158 string = value__3._1
    var t159 string = t157 + t158
    println__T_string(t159)
    return struct{}{}
}

func pair__First_int32__Second_string(first__0 int32, second__1 string) Tuple2_5int32_6string {
    var retv162 Tuple2_5int32_6string
    var t163 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: first__0,
        _1: second__1,
    }
    retv162 = t163
    return retv162
}

func println__T_string(value__1 string) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv168 string
    var t169 string = _goml_runtime_core_int32_to_string(self__6)
    retv168 = t169
    return retv168
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv171 string
    retv171 = self__38
    return retv171
}

func main() {
    main0()
}
