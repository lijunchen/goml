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
    var t162 int32 = value__0._0
    var t163 int32 = value__0._1
    var t164 int32 = t162 + t163
    return t164
}

func classify(value__1 uint8) string {
    var t169 bool = value__1 == 10
    if t169 {
        return "newline"
    } else {
        var t172 bool = value__1 >= 65
        if t172 {
            var t175 bool = value__1 <= 90
            if t175 {
                return "uppercase"
            } else {
                return "other"
            }
        } else {
            return "other"
        }
    }
}

func main0() struct{} {
    var user__2 uint64 = 255
    var pair__3 Tuple2_5int32_5int32 = Tuple2_5int32_5int32{
        _0: 10,
        _1: 10,
    }
    var marker__4 uint8 = 65
    println__T_uint64(user__2)
    var t177 int32 = sum_pair(pair__3)
    println__T_int32(t177)
    println__T_uint8(marker__4)
    var t178 string = classify(marker__4)
    println__T_string(t178)
    var t179 string = classify(10)
    println__T_string(t179)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t182 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t182)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t185 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t185)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t188 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t188)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t191)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var t195 string = _goml_runtime_core_uint64_to_string(self__48)
    return t195
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t198 string = _goml_runtime_core_int32_to_string(self__43)
    return t198
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var t201 string = _goml_runtime_core_uint8_to_string(self__45)
    return t201
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
