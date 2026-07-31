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
    var retv158 int32
    var t159 int32 = value__0._0
    var t160 int32 = value__0._1
    var t161 int32 = t159 + t160
    retv158 = t161
    return retv158
}

func classify(value__1 uint8) string {
    var retv163 string
    var match152 uint8 = value__1
    var t166 bool = match152 == 10
    var jp165 string
    if t166 {
        jp165 = "newline"
    } else {
        var t169 bool = match152 >= 65
        var jp168 string
        if t169 {
            var t172 bool = match152 <= 90
            var jp171 string
            if t172 {
                jp171 = "uppercase"
            } else {
                jp171 = "other"
            }
            jp168 = jp171
        } else {
            jp168 = "other"
        }
        jp165 = jp168
    }
    retv163 = jp165
    return retv163
}

func main0() struct{} {
    var user__2 uint64 = 255
    var pair__3 Tuple2_5int32_5int32 = Tuple2_5int32_5int32{
        _0: 10,
        _1: 10,
    }
    var marker__4 uint8 = 65
    println__T_uint64(user__2)
    var t174 int32 = sum_pair(pair__3)
    println__T_int32(t174)
    println__T_uint8(marker__4)
    var t175 string = classify(marker__4)
    println__T_string(t175)
    var t176 string = classify(10)
    println__T_string(t176)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t179 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t179)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t182 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t182)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t185 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t185)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t188 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t188)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv191 string
    var t192 string = _goml_runtime_core_uint64_to_string(self__48)
    retv191 = t192
    return retv191
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv194 string
    var t195 string = _goml_runtime_core_int32_to_string(self__43)
    retv194 = t195
    return retv194
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv197 string
    var t198 string = _goml_runtime_core_uint8_to_string(self__45)
    retv197 = t198
    return retv197
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv200 string
    retv200 = self__38
    return retv200
}

func main() {
    main0()
}
