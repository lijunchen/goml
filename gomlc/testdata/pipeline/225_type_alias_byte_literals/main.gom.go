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
    var inline485 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(user__2)
    _goml_runtime_core_string_println(inline485)
    var t430 int32
    var inline481 int32 = 10
    var inline482 int32 = 10
    var inline483 int32 = inline481 + inline482
    t430 = inline483
    var inline478 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t430)
    _goml_runtime_core_string_println(inline478)
    var inline475 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(marker__4)
    _goml_runtime_core_string_println(inline475)
    var t431 string
    var inline471 bool = marker__4 == 10
    if inline471 {
        t431 = "newline"
    } else {
        var inline472 bool = marker__4 >= 65
        if inline472 {
            var inline473 bool = marker__4 <= 90
            if inline473 {
                t431 = "uppercase"
            } else {
                t431 = "other"
            }
        } else {
            t431 = "other"
        }
    }
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t431)
    _goml_runtime_core_string_println(inline467)
    var t432 string
    var inline461 uint8 = 10
    var inline463 bool = inline461 == 10
    if inline463 {
        t432 = "newline"
    } else {
        var inline464 bool = inline461 >= 65
        if inline464 {
            var inline465 bool = inline461 <= 90
            if inline465 {
                t432 = "uppercase"
            } else {
                t432 = "other"
            }
        } else {
            t432 = "other"
        }
    }
    var inline458 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline458)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__159 uint64) string {
    var t448 string = _goml_runtime_core_uint64_to_string(self__159)
    return t448
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t451 string = _goml_runtime_core_int32_to_string(self__154)
    return t451
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__156 uint8) string {
    var t454 string = _goml_runtime_core_uint8_to_string(self__156)
    return t454
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
