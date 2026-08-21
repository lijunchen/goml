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

type Ordering int32

func main0() struct{} {
    var user__2 uint64 = 255
    var marker__4 uint8 = 65
    var inline488 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(user__2)
    _goml_runtime_core_string_println(inline488)
    var t433 int32
    var inline484 int32 = 10
    var inline485 int32 = 10
    var inline486 int32 = inline484 + inline485
    t433 = inline486
    var inline481 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t433)
    _goml_runtime_core_string_println(inline481)
    var inline478 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(marker__4)
    _goml_runtime_core_string_println(inline478)
    var t434 string
    var inline474 bool = marker__4 == 10
    if inline474 {
        t434 = "newline"
    } else {
        var inline475 bool = marker__4 >= 65
        if inline475 {
            var inline476 bool = marker__4 <= 90
            if inline476 {
                t434 = "uppercase"
            } else {
                t434 = "other"
            }
        } else {
            t434 = "other"
        }
    }
    var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline470)
    var t435 string
    var inline464 uint8 = 10
    var inline466 bool = inline464 == 10
    if inline466 {
        t435 = "newline"
    } else {
        var inline467 bool = inline464 >= 65
        if inline467 {
            var inline468 bool = inline464 <= 90
            if inline468 {
                t435 = "uppercase"
            } else {
                t435 = "other"
            }
        } else {
            t435 = "other"
        }
    }
    var inline461 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline461)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__159 uint64) string {
    var t451 string = _goml_runtime_core_uint64_to_string(self__159)
    return t451
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t454 string = _goml_runtime_core_int32_to_string(self__154)
    return t454
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__156 uint8) string {
    var t457 string = _goml_runtime_core_uint8_to_string(self__156)
    return t457
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
