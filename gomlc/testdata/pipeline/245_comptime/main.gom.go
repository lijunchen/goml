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

type Ordering int32

type Choice struct {
    _tag int32
    _v0_0 int
}

const (
    ANSWER int = 120
)

func factorial(value__0 int) int {
    var t426 bool = value__0 < 2
    if t426 {
        return 1
    } else {
        var t427 int = value__0 - 1
        var t428 int = factorial(t427)
        var t429 int = value__0 * t428
        return t429
    }
}

func main0() struct{} {
    var values__12 [3]int = [3]int{6, 10, 4}
    var inline531 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(ANSWER)
    _goml_runtime_core_string_println(inline531)
    var t461 int = array_get__Array_3_3int(values__12, 0)
    var inline528 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t461)
    _goml_runtime_core_string_println(inline528)
    var t462 int = array_get__Array_3_3int(values__12, 1)
    var inline525 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t462)
    _goml_runtime_core_string_println(inline525)
    var t463 int = array_get__Array_3_3int(values__12, 2)
    var inline522 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t463)
    _goml_runtime_core_string_println(inline522)
    var t464 int = 7
    var t465 int = 8
    var t466 int = t464 + t465
    var inline519 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t466)
    _goml_runtime_core_string_println(inline519)
    var x416 int = 9
    var inline491 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x416)
    _goml_runtime_core_string_println(inline491)
    var t468 int = factorial(5)
    var t469 bool = t468 == ANSWER
    var inline516 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t469)
    _goml_runtime_core_string_println(inline516)
    var shadowed__16 int = 12
    var inline513 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(shadowed__16)
    _goml_runtime_core_string_println(inline513)
    var t470 int
    var inline506 int = 3
    var inline508 int = inline506 + 1
    var inline509 int = inline508 * 2
    var inline511 int = inline508 + inline509
    t470 = inline511
    var t471 bool = t470 == shadowed__16
    var inline503 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t471)
    _goml_runtime_core_string_println(inline503)
    var widened__17 uint64 = 18446744073709551615
    var t472 uint64
    var inline500 int8 = -1
    var inline501 uint64 = uint64(int8(inline500))
    t472 = inline501
    var t473 bool = widened__17 == t472
    var inline497 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t473)
    _goml_runtime_core_string_println(inline497)
    var t474 int = factorial(4)
    var inline494 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t474)
    _goml_runtime_core_string_println(inline494)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t486 string = _goml_runtime_core_int_to_string(self__151)
    return t486
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t489 string = _goml_runtime_core_bool_to_string(self__148)
    return t489
}

func main() {
    main0()
}
