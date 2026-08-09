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
    var inline295 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(ANSWER)
    _goml_runtime_core_string_println(inline295)
    var t225 int = array_get__Array_3_3int(values__12, 0)
    var inline292 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t225)
    _goml_runtime_core_string_println(inline292)
    var t226 int = array_get__Array_3_3int(values__12, 1)
    var inline289 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t226)
    _goml_runtime_core_string_println(inline289)
    var t227 int = array_get__Array_3_3int(values__12, 2)
    var inline286 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t227)
    _goml_runtime_core_string_println(inline286)
    var t228 int = 7
    var t229 int = 8
    var t230 int = t228 + t229
    var inline283 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t230)
    _goml_runtime_core_string_println(inline283)
    var x180 int = 9
    var inline255 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x180)
    _goml_runtime_core_string_println(inline255)
    var t232 int = factorial(5)
    var t233 bool = t232 == ANSWER
    var inline280 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t233)
    _goml_runtime_core_string_println(inline280)
    var shadowed__16 int = 12
    var inline277 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(shadowed__16)
    _goml_runtime_core_string_println(inline277)
    var t234 int
    var inline270 int = 3
    var inline272 int = inline270 + 1
    var inline273 int = inline272 * 2
    var inline275 int = inline272 + inline273
    t234 = inline275
    var t235 bool = t234 == shadowed__16
    var inline267 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t235)
    _goml_runtime_core_string_println(inline267)
    var widened__17 uint64 = 18446744073709551615
    var t236 uint64
    var inline264 int8 = -1
    var inline265 uint64 = uint64(int8(inline264))
    t236 = inline265
    var t237 bool = widened__17 == t236
    var inline261 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t237)
    _goml_runtime_core_string_println(inline261)
    var t238 int = factorial(4)
    var inline258 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t238)
    _goml_runtime_core_string_println(inline258)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t250 string = _goml_runtime_core_int_to_string(self__69)
    return t250
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t253 string = _goml_runtime_core_bool_to_string(self__66)
    return t253
}

func main() {
    main0()
}
