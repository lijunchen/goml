package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint16_to_string(x uint16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint32_to_string(x uint32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint64_to_string(x uint64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var start8__0 uint8 = 200
    var add8__1 uint8 = 55
    var sum8__2 uint8 = start8__0 + add8__1
    var neg8__3 uint8 = -start8__0
    var start16__4 uint16 = 50000
    var add16__5 uint16 = 12000
    var sum16__6 uint16 = start16__4 + add16__5
    var diff16__7 uint16 = sum16__6 - start16__4
    var add32__9 uint32 = 123456789
    var neg32__11 uint32 = -add32__9
    var start64__12 uint64 = 6000000000
    var add64__13 uint64 = 4000000000
    var sum64__14 uint64 = start64__12 + add64__13
    var diff64__15 uint64 = sum64__14 - add64__13
    var t413 string
    var inline454 string = _goml_runtime_core_uint8_to_string(sum8__2)
    t413 = inline454
    var t414 string = t413 + ", "
    var t415 string
    var inline452 string = _goml_runtime_core_uint8_to_string(neg8__3)
    t415 = inline452
    var t416 string = t414 + t415
    var t417 string = t416 + "; "
    var t418 string
    var inline450 string = _goml_runtime_core_uint16_to_string(diff16__7)
    t418 = inline450
    var t419 string = t417 + t418
    var t420 string = t419 + "; "
    var t421 string
    var inline448 string = _goml_runtime_core_uint32_to_string(neg32__11)
    t421 = inline448
    var t422 string = t420 + t421
    var t423 string = t422 + "; "
    var t424 string
    var inline446 string = _goml_runtime_core_uint64_to_string(diff64__15)
    t424 = inline446
    var message__16 string = t423 + t424
    var inline443 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__16)
    _goml_runtime_core_string_println(inline443)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
