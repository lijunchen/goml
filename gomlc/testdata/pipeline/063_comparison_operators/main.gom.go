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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func test_int_comparisons() struct{} {
    var a__0 int32 = 10
    var b__1 int32 = 20
    var c__2 int32 = 10
    var less__3 bool = a__0 < b__1
    var t437 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t438 string = "10 < 20: " + t437
    var inline533 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline533)
    var greater__4 bool = b__1 > a__0
    var t439 string
    var inline531 string = _goml_runtime_core_bool_to_string(greater__4)
    t439 = inline531
    var t440 string = "20 > 10: " + t439
    var inline528 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
    _goml_runtime_core_string_println(inline528)
    var less_eq1__5 bool = a__0 <= b__1
    var t441 string
    var inline526 string = _goml_runtime_core_bool_to_string(less_eq1__5)
    t441 = inline526
    var t442 string = "10 <= 20: " + t441
    var inline523 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t442)
    _goml_runtime_core_string_println(inline523)
    var less_eq2__6 bool = a__0 <= c__2
    var t443 string
    var inline521 string = _goml_runtime_core_bool_to_string(less_eq2__6)
    t443 = inline521
    var t444 string = "10 <= 10: " + t443
    var inline518 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
    _goml_runtime_core_string_println(inline518)
    var greater_eq1__7 bool = b__1 >= a__0
    var t445 string
    var inline516 string = _goml_runtime_core_bool_to_string(greater_eq1__7)
    t445 = inline516
    var t446 string = "20 >= 10: " + t445
    var inline513 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t446)
    _goml_runtime_core_string_println(inline513)
    var greater_eq2__8 bool = c__2 >= a__0
    var t447 string
    var inline511 string = _goml_runtime_core_bool_to_string(greater_eq2__8)
    t447 = inline511
    var t448 string = "10 >= 10: " + t447
    var inline508 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t448)
    _goml_runtime_core_string_println(inline508)
    var eq1__9 bool = a__0 == c__2
    var t449 string
    var inline506 string = _goml_runtime_core_bool_to_string(eq1__9)
    t449 = inline506
    var t450 string = "10 == 10: " + t449
    var inline503 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t450)
    _goml_runtime_core_string_println(inline503)
    var eq2__10 bool = a__0 == b__1
    var t451 string
    var inline501 string = _goml_runtime_core_bool_to_string(eq2__10)
    t451 = inline501
    var t452 string = "10 == 20: " + t451
    var inline498 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t452)
    _goml_runtime_core_string_println(inline498)
    var neq1__11 bool = a__0 != b__1
    var t453 string
    var inline496 string = _goml_runtime_core_bool_to_string(neq1__11)
    t453 = inline496
    var t454 string = "10 != 20: " + t453
    var inline493 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t454)
    _goml_runtime_core_string_println(inline493)
    var neq2__12 bool = a__0 != c__2
    var t455 string
    var inline491 string = _goml_runtime_core_bool_to_string(neq2__12)
    t455 = inline491
    var t456 string = "10 != 10: " + t455
    var inline488 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t456)
    _goml_runtime_core_string_println(inline488)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t458 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t459 string = "2.71 < 3.14: " + t458
    var inline581 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t459)
    _goml_runtime_core_string_println(inline581)
    var greater__17 bool = x__13 > y__14
    var t460 string
    var inline579 string = _goml_runtime_core_bool_to_string(greater__17)
    t460 = inline579
    var t461 string = "3.14 > 2.71: " + t460
    var inline576 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t461)
    _goml_runtime_core_string_println(inline576)
    var less_eq1__18 bool = y__14 <= x__13
    var t462 string
    var inline574 string = _goml_runtime_core_bool_to_string(less_eq1__18)
    t462 = inline574
    var t463 string = "2.71 <= 3.14: " + t462
    var inline571 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t463)
    _goml_runtime_core_string_println(inline571)
    var less_eq2__19 bool = x__13 <= z__15
    var t464 string
    var inline569 string = _goml_runtime_core_bool_to_string(less_eq2__19)
    t464 = inline569
    var t465 string = "3.14 <= 3.14: " + t464
    var inline566 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t465)
    _goml_runtime_core_string_println(inline566)
    var greater_eq1__20 bool = x__13 >= y__14
    var t466 string
    var inline564 string = _goml_runtime_core_bool_to_string(greater_eq1__20)
    t466 = inline564
    var t467 string = "3.14 >= 2.71: " + t466
    var inline561 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t467)
    _goml_runtime_core_string_println(inline561)
    var greater_eq2__21 bool = z__15 >= x__13
    var t468 string
    var inline559 string = _goml_runtime_core_bool_to_string(greater_eq2__21)
    t468 = inline559
    var t469 string = "3.14 >= 3.14: " + t468
    var inline556 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t469)
    _goml_runtime_core_string_println(inline556)
    var eq1__22 bool = x__13 == z__15
    var t470 string
    var inline554 string = _goml_runtime_core_bool_to_string(eq1__22)
    t470 = inline554
    var t471 string = "3.14 == 3.14: " + t470
    var inline551 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t471)
    _goml_runtime_core_string_println(inline551)
    var eq2__23 bool = x__13 == y__14
    var t472 string
    var inline549 string = _goml_runtime_core_bool_to_string(eq2__23)
    t472 = inline549
    var t473 string = "3.14 == 2.71: " + t472
    var inline546 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t473)
    _goml_runtime_core_string_println(inline546)
    var neq1__24 bool = x__13 != y__14
    var t474 string
    var inline544 string = _goml_runtime_core_bool_to_string(neq1__24)
    t474 = inline544
    var t475 string = "3.14 != 2.71: " + t474
    var inline541 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t475)
    _goml_runtime_core_string_println(inline541)
    var neq2__25 bool = x__13 != z__15
    var t476 string
    var inline539 string = _goml_runtime_core_bool_to_string(neq2__25)
    t476 = inline539
    var t477 string = "3.14 != 3.14: " + t476
    var inline536 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t477)
    _goml_runtime_core_string_println(inline536)
    return struct{}{}
}

func main0() struct{} {
    var inline592 string = "=== Integer Comparisons ==="
    var inline593 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline592)
    _goml_runtime_core_string_println(inline593)
    test_int_comparisons()
    var inline588 string = ""
    var inline589 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline588)
    _goml_runtime_core_string_println(inline589)
    var inline584 string = "=== Float Comparisons ==="
    var inline585 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline584)
    _goml_runtime_core_string_println(inline585)
    test_float_comparisons()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t484 string = _goml_runtime_core_bool_to_string(self__148)
    return t484
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
