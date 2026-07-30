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
    var t109 int32 = value__3._0
    var t110 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t109)
    var t111 string = value__3._1
    var t112 string = t110 + t111
    println__T_string(t112)
    return struct{}{}
}

func pair__First_int32__Second_string(first__0 int32, second__1 string) Tuple2_5int32_6string {
    var retv115 Tuple2_5int32_6string
    var t116 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: first__0,
        _1: second__1,
    }
    retv115 = t116
    return retv115
}

func println__T_string(value__1 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t118)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv121 string
    var t122 string = _goml_runtime_core_int32_to_string(self__6)
    retv121 = t122
    return retv121
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv124 string
    retv124 = self__38
    return retv124
}

func main() {
    main0()
}
