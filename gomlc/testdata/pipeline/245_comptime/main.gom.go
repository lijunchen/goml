package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_3_3int(arr [3]int, index int) int {
    return arr[index]
}

type Pair struct {
    left int
    right int
}

type Choice interface {
    isChoice()
}

type Value struct {
    _0 int
}

func (_ Value) isChoice() {}

type Empty struct {}

func (_ Empty) isChoice() {}

const (
    ANSWER int = 120
)

func factorial(value__0 int) int {
    var t200 bool = value__0 < 2
    if t200 {
        return 1
    } else {
        var t201 int = value__0 - 1
        var t202 int = factorial(t201)
        var t203 int = value__0 * t202
        return t203
    }
}

func main0() struct{} {
    var values__12 [3]int = [3]int{6, 10, 4}
    var inline305 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(ANSWER)
    _goml_runtime_core_string_println(inline305)
    var t235 int = array_get__Array_3_3int(values__12, 0)
    var inline302 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t235)
    _goml_runtime_core_string_println(inline302)
    var t236 int = array_get__Array_3_3int(values__12, 1)
    var inline299 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t236)
    _goml_runtime_core_string_println(inline299)
    var t237 int = array_get__Array_3_3int(values__12, 2)
    var inline296 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t237)
    _goml_runtime_core_string_println(inline296)
    var t238 int = 7
    var t239 int = 8
    var t240 int = t238 + t239
    var inline293 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t240)
    _goml_runtime_core_string_println(inline293)
    var x190 int = 9
    var inline265 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x190)
    _goml_runtime_core_string_println(inline265)
    var t242 int = factorial(5)
    var t243 bool = t242 == ANSWER
    var inline290 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t243)
    _goml_runtime_core_string_println(inline290)
    var shadowed__16 int = 12
    var inline287 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(shadowed__16)
    _goml_runtime_core_string_println(inline287)
    var t244 int
    var inline280 int = 3
    var inline282 int = inline280 + 1
    var inline283 int = inline282 * 2
    var inline285 int = inline282 + inline283
    t244 = inline285
    var t245 bool = t244 == shadowed__16
    var inline277 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t245)
    _goml_runtime_core_string_println(inline277)
    var widened__17 uint64 = 18446744073709551615
    var t246 uint64
    var inline274 int8 = -1
    var inline275 uint64 = uint64(int8(inline274))
    t246 = inline275
    var t247 bool = widened__17 == t246
    var inline271 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t247)
    _goml_runtime_core_string_println(inline271)
    var t248 int = factorial(4)
    var inline268 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t248)
    _goml_runtime_core_string_println(inline268)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t260 string = _goml_runtime_core_int_to_string(self__67)
    return t260
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t263 string = _goml_runtime_core_bool_to_string(self__64)
    return t263
}

func main() {
    main0()
}
