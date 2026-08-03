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
    var inline254 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(user__2)
    _goml_runtime_core_string_println(inline254)
    var t199 int32
    var inline250 int32 = 10
    var inline251 int32 = 10
    var inline252 int32 = inline250 + inline251
    t199 = inline252
    var inline247 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
    _goml_runtime_core_string_println(inline247)
    var inline244 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(marker__4)
    _goml_runtime_core_string_println(inline244)
    var t200 string
    var inline240 bool = marker__4 == 10
    if inline240 {
        t200 = "newline"
    } else {
        var inline241 bool = marker__4 >= 65
        if inline241 {
            var inline242 bool = marker__4 <= 90
            if inline242 {
                t200 = "uppercase"
            } else {
                t200 = "other"
            }
        } else {
            t200 = "other"
        }
    }
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline236)
    var t201 string
    var inline230 uint8 = 10
    var inline232 bool = inline230 == 10
    if inline232 {
        t201 = "newline"
    } else {
        var inline233 bool = inline230 >= 65
        if inline233 {
            var inline234 bool = inline230 <= 90
            if inline234 {
                t201 = "uppercase"
            } else {
                t201 = "other"
            }
        } else {
            t201 = "other"
        }
    }
    var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline227)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__77 uint64) string {
    var t217 string = _goml_runtime_core_uint64_to_string(self__77)
    return t217
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t220 string = _goml_runtime_core_int32_to_string(self__72)
    return t220
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__74 uint8) string {
    var t223 string = _goml_runtime_core_uint8_to_string(self__74)
    return t223
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
