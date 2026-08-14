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
    var inline264 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(user__2)
    _goml_runtime_core_string_println(inline264)
    var t209 int32
    var inline260 int32 = 10
    var inline261 int32 = 10
    var inline262 int32 = inline260 + inline261
    t209 = inline262
    var inline257 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
    _goml_runtime_core_string_println(inline257)
    var inline254 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(marker__4)
    _goml_runtime_core_string_println(inline254)
    var t210 string
    var inline250 bool = marker__4 == 10
    if inline250 {
        t210 = "newline"
    } else {
        var inline251 bool = marker__4 >= 65
        if inline251 {
            var inline252 bool = marker__4 <= 90
            if inline252 {
                t210 = "uppercase"
            } else {
                t210 = "other"
            }
        } else {
            t210 = "other"
        }
    }
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline246)
    var t211 string
    var inline240 uint8 = 10
    var inline242 bool = inline240 == 10
    if inline242 {
        t211 = "newline"
    } else {
        var inline243 bool = inline240 >= 65
        if inline243 {
            var inline244 bool = inline240 <= 90
            if inline244 {
                t211 = "uppercase"
            } else {
                t211 = "other"
            }
        } else {
            t211 = "other"
        }
    }
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline237)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__75 uint64) string {
    var t227 string = _goml_runtime_core_uint64_to_string(self__75)
    return t227
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t230 string = _goml_runtime_core_int32_to_string(self__70)
    return t230
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__72 uint8) string {
    var t233 string = _goml_runtime_core_uint8_to_string(self__72)
    return t233
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
