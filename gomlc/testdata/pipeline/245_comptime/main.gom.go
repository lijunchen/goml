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
    answer int = 120
)

func factorial(value__0 int) int {
    var t190 bool = value__0 < 2
    if t190 {
        return 1
    } else {
        var t191 int = value__0 - 1
        var t192 int = factorial(t191)
        var t193 int = value__0 * t192
        return t193
    }
}

func main0() struct{} {
    var values__12 [3]int = [3]int{6, 10, 4}
    var inline307 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(answer)
    _goml_runtime_core_string_println(inline307)
    var t225 int = array_get__Array_3_3int(values__12, 0)
    var inline304 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t225)
    _goml_runtime_core_string_println(inline304)
    var t226 int = array_get__Array_3_3int(values__12, 1)
    var inline301 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t226)
    _goml_runtime_core_string_println(inline301)
    var t227 int = array_get__Array_3_3int(values__12, 2)
    var inline298 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t227)
    _goml_runtime_core_string_println(inline298)
    var t228 int = 7
    var t229 int = 8
    var t230 int = t228 + t229
    var inline295 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t230)
    _goml_runtime_core_string_println(inline295)
    var x180 int = 9
    var inline261 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x180)
    _goml_runtime_core_string_println(inline261)
    var t232 int = factorial(5)
    var t233 bool
    var inline293 bool = t232 == answer
    t233 = inline293
    var inline290 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t233)
    _goml_runtime_core_string_println(inline290)
    var shadowed__16 int = 12
    var inline287 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(shadowed__16)
    _goml_runtime_core_string_println(inline287)
    var t234 int
    var inline280 int = 3
    var inline282 int = inline280 + 1
    var inline283 int = inline282 * 2
    var inline285 int = inline282 + inline283
    t234 = inline285
    var t235 bool
    var inline278 bool = t234 == shadowed__16
    t235 = inline278
    var inline275 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t235)
    _goml_runtime_core_string_println(inline275)
    var widened__17 uint64 = 18446744073709551615
    var t236 uint64
    var inline272 int8 = -1
    var inline273 uint64 = uint64(int8(inline272))
    t236 = inline273
    var t237 bool
    var inline270 bool = widened__17 == t236
    t237 = inline270
    var inline267 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t237)
    _goml_runtime_core_string_println(inline267)
    var t238 int = factorial(4)
    var inline264 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t238)
    _goml_runtime_core_string_println(inline264)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t256 string = _goml_runtime_core_int_to_string(self__69)
    return t256
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t259 string = _goml_runtime_core_bool_to_string(self__66)
    return t259
}

func main() {
    main0()
}
