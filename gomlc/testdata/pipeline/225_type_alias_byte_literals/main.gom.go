package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint64_to_string(x uint64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_5int32_5int32 struct {
    _0 int32
    _1 int32
}

func sum_pair(value__0 Tuple2_5int32_5int32) int32 {
    var retv74 int32
    var t75 int32 = value__0._0
    var t76 int32 = value__0._1
    var t77 int32 = t75 + t76
    retv74 = t77
    return retv74
}

func classify(value__1 uint8) string {
    var retv79 string
    var match68 uint8 = value__1
    var t82 bool = match68 == 10
    var jp81 string
    if t82 {
        jp81 = "newline"
    } else {
        var t85 bool = match68 >= 65
        var jp84 string
        if t85 {
            var t88 bool = match68 <= 90
            var jp87 string
            if t88 {
                jp87 = "uppercase"
            } else {
                jp87 = "other"
            }
            jp84 = jp87
        } else {
            jp84 = "other"
        }
        jp81 = jp84
    }
    retv79 = jp81
    return retv79
}

func main0() struct{} {
    var user__2 uint64 = 255
    var pair__3 Tuple2_5int32_5int32 = Tuple2_5int32_5int32{
        _0: 10,
        _1: 10,
    }
    var marker__4 uint8 = 65
    println__T_uint64(user__2)
    var t90 int32 = sum_pair(pair__3)
    println__T_int32(t90)
    println__T_uint8(marker__4)
    var t91 string = classify(marker__4)
    println__T_string(t91)
    var t92 string = classify(10)
    println__T_string(t92)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t104 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t104)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv107 string
    var t108 string = _goml_runtime_core_uint64_to_string(self__48)
    retv107 = t108
    return retv107
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv110 string
    var t111 string = _goml_runtime_core_int32_to_string(self__43)
    retv110 = t111
    return retv110
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv113 string
    var t114 string = _goml_runtime_core_uint8_to_string(self__45)
    retv113 = t114
    return retv113
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv116 string
    retv116 = self__38
    return retv116
}

func main() {
    main0()
}
