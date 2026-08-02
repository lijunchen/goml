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

func main0() struct{} {
    var user__2 uint64 = 255
    var marker__4 uint8 = 65
    var inline232 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(user__2)
    _goml_runtime_core_string_println(inline232)
    var t177 int32
    var inline228 int32 = 10
    var inline229 int32 = 10
    var inline230 int32 = inline228 + inline229
    t177 = inline230
    var inline225 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t177)
    _goml_runtime_core_string_println(inline225)
    var inline222 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(marker__4)
    _goml_runtime_core_string_println(inline222)
    var t178 string
    var inline218 bool = marker__4 == 10
    if inline218 {
        t178 = "newline"
    } else {
        var inline219 bool = marker__4 >= 65
        if inline219 {
            var inline220 bool = marker__4 <= 90
            if inline220 {
                t178 = "uppercase"
            } else {
                t178 = "other"
            }
        } else {
            t178 = "other"
        }
    }
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
    _goml_runtime_core_string_println(inline214)
    var t179 string
    var inline208 uint8 = 10
    var inline210 bool = inline208 == 10
    if inline210 {
        t179 = "newline"
    } else {
        var inline211 bool = inline208 >= 65
        if inline211 {
            var inline212 bool = inline208 <= 90
            if inline212 {
                t179 = "uppercase"
            } else {
                t179 = "other"
            }
        } else {
            t179 = "other"
        }
    }
    var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
    _goml_runtime_core_string_println(inline205)
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
