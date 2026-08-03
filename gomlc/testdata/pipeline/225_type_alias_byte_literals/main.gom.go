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
    var inline213 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(user__2)
    _goml_runtime_core_string_println(inline213)
    var t158 int32
    var inline209 int32 = 10
    var inline210 int32 = 10
    var inline211 int32 = inline209 + inline210
    t158 = inline211
    var inline206 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t158)
    _goml_runtime_core_string_println(inline206)
    var inline203 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(marker__4)
    _goml_runtime_core_string_println(inline203)
    var t159 string
    var inline199 bool = marker__4 == 10
    if inline199 {
        t159 = "newline"
    } else {
        var inline200 bool = marker__4 >= 65
        if inline200 {
            var inline201 bool = marker__4 <= 90
            if inline201 {
                t159 = "uppercase"
            } else {
                t159 = "other"
            }
        } else {
            t159 = "other"
        }
    }
    var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
    _goml_runtime_core_string_println(inline195)
    var t160 string
    var inline189 uint8 = 10
    var inline191 bool = inline189 == 10
    if inline191 {
        t160 = "newline"
    } else {
        var inline192 bool = inline189 >= 65
        if inline192 {
            var inline193 bool = inline189 <= 90
            if inline193 {
                t160 = "uppercase"
            } else {
                t160 = "other"
            }
        } else {
            t160 = "other"
        }
    }
    var inline186 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline186)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__77 uint64) string {
    var t176 string = _goml_runtime_core_uint64_to_string(self__77)
    return t176
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t179 string = _goml_runtime_core_int32_to_string(self__72)
    return t179
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__74 uint8) string {
    var t182 string = _goml_runtime_core_uint8_to_string(self__74)
    return t182
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
