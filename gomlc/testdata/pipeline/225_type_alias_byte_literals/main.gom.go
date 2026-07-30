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
    var retv114 int32
    var t115 int32 = value__0._0
    var t116 int32 = value__0._1
    var t117 int32 = t115 + t116
    retv114 = t117
    return retv114
}

func classify(value__1 uint8) string {
    var retv119 string
    var match108 uint8 = value__1
    var t122 bool = match108 == 10
    var jp121 string
    if t122 {
        jp121 = "newline"
    } else {
        var t125 bool = match108 >= 65
        var jp124 string
        if t125 {
            var t128 bool = match108 <= 90
            var jp127 string
            if t128 {
                jp127 = "uppercase"
            } else {
                jp127 = "other"
            }
            jp124 = jp127
        } else {
            jp124 = "other"
        }
        jp121 = jp124
    }
    retv119 = jp121
    return retv119
}

func main0() struct{} {
    var user__2 uint64 = 255
    var pair__3 Tuple2_5int32_5int32 = Tuple2_5int32_5int32{
        _0: 10,
        _1: 10,
    }
    var marker__4 uint8 = 65
    println__T_uint64(user__2)
    var t130 int32 = sum_pair(pair__3)
    println__T_int32(t130)
    println__T_uint8(marker__4)
    var t131 string = classify(marker__4)
    println__T_string(t131)
    var t132 string = classify(10)
    println__T_string(t132)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t135 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t135)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t138 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t138)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t141 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t141)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t144 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t144)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv147 string
    var t148 string = _goml_runtime_core_uint64_to_string(self__48)
    retv147 = t148
    return retv147
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv150 string
    var t151 string = _goml_runtime_core_int32_to_string(self__43)
    retv150 = t151
    return retv150
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv153 string
    var t154 string = _goml_runtime_core_uint8_to_string(self__45)
    retv153 = t154
    return retv153
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv156 string
    retv156 = self__38
    return retv156
}

func main() {
    main0()
}
