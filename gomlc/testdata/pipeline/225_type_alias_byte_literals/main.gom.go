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
    var inline249 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(user__2)
    _goml_runtime_core_string_println(inline249)
    var t194 int32
    var inline245 int32 = 10
    var inline246 int32 = 10
    var inline247 int32 = inline245 + inline246
    t194 = inline247
    var inline242 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t194)
    _goml_runtime_core_string_println(inline242)
    var inline239 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(marker__4)
    _goml_runtime_core_string_println(inline239)
    var t195 string
    var inline235 bool = marker__4 == 10
    if inline235 {
        t195 = "newline"
    } else {
        var inline236 bool = marker__4 >= 65
        if inline236 {
            var inline237 bool = marker__4 <= 90
            if inline237 {
                t195 = "uppercase"
            } else {
                t195 = "other"
            }
        } else {
            t195 = "other"
        }
    }
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline231)
    var t196 string
    var inline225 uint8 = 10
    var inline227 bool = inline225 == 10
    if inline227 {
        t196 = "newline"
    } else {
        var inline228 bool = inline225 >= 65
        if inline228 {
            var inline229 bool = inline225 <= 90
            if inline229 {
                t196 = "uppercase"
            } else {
                t196 = "other"
            }
        } else {
            t196 = "other"
        }
    }
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline222)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__77 uint64) string {
    var t212 string = _goml_runtime_core_uint64_to_string(self__77)
    return t212
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t215 string = _goml_runtime_core_int32_to_string(self__72)
    return t215
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__74 uint8) string {
    var t218 string = _goml_runtime_core_uint8_to_string(self__74)
    return t218
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
