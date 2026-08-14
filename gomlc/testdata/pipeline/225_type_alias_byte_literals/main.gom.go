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
    var inline259 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(user__2)
    _goml_runtime_core_string_println(inline259)
    var t204 int32
    var inline255 int32 = 10
    var inline256 int32 = 10
    var inline257 int32 = inline255 + inline256
    t204 = inline257
    var inline252 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t204)
    _goml_runtime_core_string_println(inline252)
    var inline249 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(marker__4)
    _goml_runtime_core_string_println(inline249)
    var t205 string
    var inline245 bool = marker__4 == 10
    if inline245 {
        t205 = "newline"
    } else {
        var inline246 bool = marker__4 >= 65
        if inline246 {
            var inline247 bool = marker__4 <= 90
            if inline247 {
                t205 = "uppercase"
            } else {
                t205 = "other"
            }
        } else {
            t205 = "other"
        }
    }
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline241)
    var t206 string
    var inline235 uint8 = 10
    var inline237 bool = inline235 == 10
    if inline237 {
        t206 = "newline"
    } else {
        var inline238 bool = inline235 >= 65
        if inline238 {
            var inline239 bool = inline235 <= 90
            if inline239 {
                t206 = "uppercase"
            } else {
                t206 = "other"
            }
        } else {
            t206 = "other"
        }
    }
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline232)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__75 uint64) string {
    var t222 string = _goml_runtime_core_uint64_to_string(self__75)
    return t222
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t225 string = _goml_runtime_core_int32_to_string(self__70)
    return t225
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__72 uint8) string {
    var t228 string = _goml_runtime_core_uint8_to_string(self__72)
    return t228
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
