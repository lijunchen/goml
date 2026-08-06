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
    var t154 bool = value__0 < 2
    if t154 {
        return 1
    } else {
        var t155 int = value__0 - 1
        var t156 int = factorial(t155)
        var t157 int = value__0 * t156
        return t157
    }
}

func main0() struct{} {
    var values__12 [3]int = [3]int{6, 10, 4}
    var inline271 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(answer)
    _goml_runtime_core_string_println(inline271)
    var t189 int = array_get__Array_3_3int(values__12, 0)
    var inline268 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t189)
    _goml_runtime_core_string_println(inline268)
    var t190 int = array_get__Array_3_3int(values__12, 1)
    var inline265 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t190)
    _goml_runtime_core_string_println(inline265)
    var t191 int = array_get__Array_3_3int(values__12, 2)
    var inline262 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t191)
    _goml_runtime_core_string_println(inline262)
    var t192 int = 7
    var t193 int = 8
    var t194 int = t192 + t193
    var inline259 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t194)
    _goml_runtime_core_string_println(inline259)
    var x144 int = 9
    var inline225 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x144)
    _goml_runtime_core_string_println(inline225)
    var t196 int = factorial(5)
    var t197 bool
    var inline257 bool = t196 == answer
    t197 = inline257
    var inline254 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t197)
    _goml_runtime_core_string_println(inline254)
    var shadowed__16 int = 12
    var inline251 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(shadowed__16)
    _goml_runtime_core_string_println(inline251)
    var t198 int
    var inline244 int = 3
    var inline246 int = inline244 + 1
    var inline247 int = inline246 * 2
    var inline249 int = inline246 + inline247
    t198 = inline249
    var t199 bool
    var inline242 bool = t198 == shadowed__16
    t199 = inline242
    var inline239 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t199)
    _goml_runtime_core_string_println(inline239)
    var widened__17 uint64 = 18446744073709551615
    var t200 uint64
    var inline236 int8 = -1
    var inline237 uint64 = uint64(int8(inline236))
    t200 = inline237
    var t201 bool
    var inline234 bool = widened__17 == t200
    t201 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t201)
    _goml_runtime_core_string_println(inline231)
    var t202 int = factorial(4)
    var inline228 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t202)
    _goml_runtime_core_string_println(inline228)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t220 string = _goml_runtime_core_int_to_string(self__69)
    return t220
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t223 string = _goml_runtime_core_bool_to_string(self__66)
    return t223
}

func main() {
    main0()
}
