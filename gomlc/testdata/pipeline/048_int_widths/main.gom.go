package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var start16__0 int16 = 300
    var delta16__1 int16 = 45
    var sum16__2 int16 = start16__0 + delta16__1
    var flipped16__3 int16 = -start16__0
    var base32__4 int32 = 100000
    var more32__5 int32 = 200000
    var sum32__6 int32 = base32__4 + more32__5
    var diff32__7 int32 = sum32__6 - base32__4
    var big64__8 int64 = 5000000000
    var step64__9 int64 = 2000000000
    var remain64__10 int64 = big64__8 - step64__9
    var neg64__11 int64 = -step64__9
    var t410 string
    var inline448 string = _goml_runtime_core_int16_to_string(sum16__2)
    t410 = inline448
    var t411 string = t410 + ", "
    var t412 string
    var inline446 string = _goml_runtime_core_int16_to_string(flipped16__3)
    t412 = inline446
    var t413 string = t411 + t412
    var t414 string = t413 + "; "
    var t415 string
    var inline444 string = _goml_runtime_core_int32_to_string(diff32__7)
    t415 = inline444
    var t416 string = t414 + t415
    var t417 string = t416 + "; "
    var t418 string
    var inline442 string = _goml_runtime_core_int64_to_string(remain64__10)
    t418 = inline442
    var t419 string = t417 + t418
    var t420 string = t419 + "; "
    var t421 string
    var inline440 string = _goml_runtime_core_int64_to_string(neg64__11)
    t421 = inline440
    var message__12 string = t420 + t421
    var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline437)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
