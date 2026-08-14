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
    var t205 bool = value__0 < 2
    if t205 {
        return 1
    } else {
        var t206 int = value__0 - 1
        var t207 int = factorial(t206)
        var t208 int = value__0 * t207
        return t208
    }
}

func main0() struct{} {
    var values__12 [3]int = [3]int{6, 10, 4}
    var inline310 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(ANSWER)
    _goml_runtime_core_string_println(inline310)
    var t240 int = array_get__Array_3_3int(values__12, 0)
    var inline307 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t240)
    _goml_runtime_core_string_println(inline307)
    var t241 int = array_get__Array_3_3int(values__12, 1)
    var inline304 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t241)
    _goml_runtime_core_string_println(inline304)
    var t242 int = array_get__Array_3_3int(values__12, 2)
    var inline301 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t242)
    _goml_runtime_core_string_println(inline301)
    var t243 int = 7
    var t244 int = 8
    var t245 int = t243 + t244
    var inline298 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t245)
    _goml_runtime_core_string_println(inline298)
    var x195 int = 9
    var inline270 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x195)
    _goml_runtime_core_string_println(inline270)
    var t247 int = factorial(5)
    var t248 bool = t247 == ANSWER
    var inline295 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t248)
    _goml_runtime_core_string_println(inline295)
    var shadowed__16 int = 12
    var inline292 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(shadowed__16)
    _goml_runtime_core_string_println(inline292)
    var t249 int
    var inline285 int = 3
    var inline287 int = inline285 + 1
    var inline288 int = inline287 * 2
    var inline290 int = inline287 + inline288
    t249 = inline290
    var t250 bool = t249 == shadowed__16
    var inline282 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t250)
    _goml_runtime_core_string_println(inline282)
    var widened__17 uint64 = 18446744073709551615
    var t251 uint64
    var inline279 int8 = -1
    var inline280 uint64 = uint64(int8(inline279))
    t251 = inline280
    var t252 bool = widened__17 == t251
    var inline276 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t252)
    _goml_runtime_core_string_println(inline276)
    var t253 int = factorial(4)
    var inline273 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t253)
    _goml_runtime_core_string_println(inline273)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t265 string = _goml_runtime_core_int_to_string(self__67)
    return t265
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t268 string = _goml_runtime_core_bool_to_string(self__64)
    return t268
}

func main() {
    main0()
}
