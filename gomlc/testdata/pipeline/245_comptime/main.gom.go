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
    var t429 bool = value__0 < 2
    if t429 {
        return 1
    } else {
        var t430 int = value__0 - 1
        var t431 int = factorial(t430)
        var t432 int = value__0 * t431
        return t432
    }
}

func main0() struct{} {
    var values__12 [3]int = [3]int{6, 10, 4}
    var inline534 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(ANSWER)
    _goml_runtime_core_string_println(inline534)
    var t464 int = array_get__Array_3_3int(values__12, 0)
    var inline531 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t464)
    _goml_runtime_core_string_println(inline531)
    var t465 int = array_get__Array_3_3int(values__12, 1)
    var inline528 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t465)
    _goml_runtime_core_string_println(inline528)
    var t466 int = array_get__Array_3_3int(values__12, 2)
    var inline525 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t466)
    _goml_runtime_core_string_println(inline525)
    var t467 int = 7
    var t468 int = 8
    var t469 int = t467 + t468
    var inline522 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t469)
    _goml_runtime_core_string_println(inline522)
    var x419 int = 9
    var inline494 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x419)
    _goml_runtime_core_string_println(inline494)
    var t471 int = factorial(5)
    var t472 bool = t471 == ANSWER
    var inline519 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t472)
    _goml_runtime_core_string_println(inline519)
    var shadowed__16 int = 12
    var inline516 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(shadowed__16)
    _goml_runtime_core_string_println(inline516)
    var t473 int
    var inline509 int = 3
    var inline511 int = inline509 + 1
    var inline512 int = inline511 * 2
    var inline514 int = inline511 + inline512
    t473 = inline514
    var t474 bool = t473 == shadowed__16
    var inline506 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t474)
    _goml_runtime_core_string_println(inline506)
    var widened__17 uint64 = 18446744073709551615
    var t475 uint64
    var inline503 int8 = -1
    var inline504 uint64 = uint64(int8(inline503))
    t475 = inline504
    var t476 bool = widened__17 == t475
    var inline500 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t476)
    _goml_runtime_core_string_println(inline500)
    var t477 int = factorial(4)
    var inline497 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t477)
    _goml_runtime_core_string_println(inline497)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t489 string = _goml_runtime_core_int_to_string(self__151)
    return t489
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t492 string = _goml_runtime_core_bool_to_string(self__148)
    return t492
}

func main() {
    main0()
}
