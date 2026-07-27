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

func pair(first__0 int32, second__1 string) Tuple2_5int32_6string {
    var retv65 Tuple2_5int32_6string
    var t66 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: first__0,
        _1: second__1,
    }
    retv65 = t66
    return retv65
}

func main0() struct{} {
    var make__2 func(int32, string) Tuple2_5int32_6string = pair
    var value__3 Tuple2_5int32_6string = make__2(1, "x")
    var t68 int32 = value__3._0
    var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t68)
    var t70 string = value__3._1
    var t71 string = t69 + t70
    println__T_string(t71)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t74 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv77 string
    var t78 string = _goml_runtime_core_int32_to_string(self__6)
    retv77 = t78
    return retv77
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv80 string
    retv80 = self__38
    return retv80
}

func main() {
    main0()
}
