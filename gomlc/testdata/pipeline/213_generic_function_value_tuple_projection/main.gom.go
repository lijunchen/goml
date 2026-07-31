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
    var t153 int32 = value__3._0
    var t154 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t153)
    var t155 string = value__3._1
    var t156 string = t154 + t155
    println__T_string(t156)
    return struct{}{}
}

func pair__First_int32__Second_string(first__0 int32, second__1 string) Tuple2_5int32_6string {
    var retv159 Tuple2_5int32_6string
    var t160 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: first__0,
        _1: second__1,
    }
    retv159 = t160
    return retv159
}

func println__T_string(value__1 string) struct{} {
    var t162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t162)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv165 string
    var t166 string = _goml_runtime_core_int32_to_string(self__6)
    retv165 = t166
    return retv165
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv168 string
    retv168 = self__38
    return retv168
}

func main() {
    main0()
}
