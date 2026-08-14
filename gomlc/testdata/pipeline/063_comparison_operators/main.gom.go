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
    var t434 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t435 string = "10 < 20: " + t434
    var inline530 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline530)
    var greater__4 bool = b__1 > a__0
    var t436 string
    var inline528 string = _goml_runtime_core_bool_to_string(greater__4)
    t436 = inline528
    var t437 string = "20 > 10: " + t436
    var inline525 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline525)
    var less_eq1__5 bool = a__0 <= b__1
    var t438 string
    var inline523 string = _goml_runtime_core_bool_to_string(less_eq1__5)
    t438 = inline523
    var t439 string = "10 <= 20: " + t438
    var inline520 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
    _goml_runtime_core_string_println(inline520)
    var less_eq2__6 bool = a__0 <= c__2
    var t440 string
    var inline518 string = _goml_runtime_core_bool_to_string(less_eq2__6)
    t440 = inline518
    var t441 string = "10 <= 10: " + t440
    var inline515 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline515)
    var greater_eq1__7 bool = b__1 >= a__0
    var t442 string
    var inline513 string = _goml_runtime_core_bool_to_string(greater_eq1__7)
    t442 = inline513
    var t443 string = "20 >= 10: " + t442
    var inline510 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline510)
    var greater_eq2__8 bool = c__2 >= a__0
    var t444 string
    var inline508 string = _goml_runtime_core_bool_to_string(greater_eq2__8)
    t444 = inline508
    var t445 string = "10 >= 10: " + t444
    var inline505 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t445)
    _goml_runtime_core_string_println(inline505)
    var eq1__9 bool = a__0 == c__2
    var t446 string
    var inline503 string = _goml_runtime_core_bool_to_string(eq1__9)
    t446 = inline503
    var t447 string = "10 == 10: " + t446
    var inline500 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t447)
    _goml_runtime_core_string_println(inline500)
    var eq2__10 bool = a__0 == b__1
    var t448 string
    var inline498 string = _goml_runtime_core_bool_to_string(eq2__10)
    t448 = inline498
    var t449 string = "10 == 20: " + t448
    var inline495 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t449)
    _goml_runtime_core_string_println(inline495)
    var neq1__11 bool = a__0 != b__1
    var t450 string
    var inline493 string = _goml_runtime_core_bool_to_string(neq1__11)
    t450 = inline493
    var t451 string = "10 != 20: " + t450
    var inline490 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t451)
    _goml_runtime_core_string_println(inline490)
    var neq2__12 bool = a__0 != c__2
    var t452 string
    var inline488 string = _goml_runtime_core_bool_to_string(neq2__12)
    t452 = inline488
    var t453 string = "10 != 10: " + t452
    var inline485 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t453)
    _goml_runtime_core_string_println(inline485)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t455 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t456 string = "2.71 < 3.14: " + t455
    var inline578 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t456)
    _goml_runtime_core_string_println(inline578)
    var greater__17 bool = x__13 > y__14
    var t457 string
    var inline576 string = _goml_runtime_core_bool_to_string(greater__17)
    t457 = inline576
    var t458 string = "3.14 > 2.71: " + t457
    var inline573 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t458)
    _goml_runtime_core_string_println(inline573)
    var less_eq1__18 bool = y__14 <= x__13
    var t459 string
    var inline571 string = _goml_runtime_core_bool_to_string(less_eq1__18)
    t459 = inline571
    var t460 string = "2.71 <= 3.14: " + t459
    var inline568 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t460)
    _goml_runtime_core_string_println(inline568)
    var less_eq2__19 bool = x__13 <= z__15
    var t461 string
    var inline566 string = _goml_runtime_core_bool_to_string(less_eq2__19)
    t461 = inline566
    var t462 string = "3.14 <= 3.14: " + t461
    var inline563 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t462)
    _goml_runtime_core_string_println(inline563)
    var greater_eq1__20 bool = x__13 >= y__14
    var t463 string
    var inline561 string = _goml_runtime_core_bool_to_string(greater_eq1__20)
    t463 = inline561
    var t464 string = "3.14 >= 2.71: " + t463
    var inline558 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t464)
    _goml_runtime_core_string_println(inline558)
    var greater_eq2__21 bool = z__15 >= x__13
    var t465 string
    var inline556 string = _goml_runtime_core_bool_to_string(greater_eq2__21)
    t465 = inline556
    var t466 string = "3.14 >= 3.14: " + t465
    var inline553 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t466)
    _goml_runtime_core_string_println(inline553)
    var eq1__22 bool = x__13 == z__15
    var t467 string
    var inline551 string = _goml_runtime_core_bool_to_string(eq1__22)
    t467 = inline551
    var t468 string = "3.14 == 3.14: " + t467
    var inline548 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t468)
    _goml_runtime_core_string_println(inline548)
    var eq2__23 bool = x__13 == y__14
    var t469 string
    var inline546 string = _goml_runtime_core_bool_to_string(eq2__23)
    t469 = inline546
    var t470 string = "3.14 == 2.71: " + t469
    var inline543 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t470)
    _goml_runtime_core_string_println(inline543)
    var neq1__24 bool = x__13 != y__14
    var t471 string
    var inline541 string = _goml_runtime_core_bool_to_string(neq1__24)
    t471 = inline541
    var t472 string = "3.14 != 2.71: " + t471
    var inline538 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t472)
    _goml_runtime_core_string_println(inline538)
    var neq2__25 bool = x__13 != z__15
    var t473 string
    var inline536 string = _goml_runtime_core_bool_to_string(neq2__25)
    t473 = inline536
    var t474 string = "3.14 != 3.14: " + t473
    var inline533 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t474)
    _goml_runtime_core_string_println(inline533)
    return struct{}{}
}

func main0() struct{} {
    var inline589 string = "=== Integer Comparisons ==="
    var inline590 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline589)
    _goml_runtime_core_string_println(inline590)
    test_int_comparisons()
    var inline585 string = ""
    var inline586 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline585)
    _goml_runtime_core_string_println(inline586)
    var inline581 string = "=== Float Comparisons ==="
    var inline582 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline581)
    _goml_runtime_core_string_println(inline582)
    test_float_comparisons()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t481 string = _goml_runtime_core_bool_to_string(self__148)
    return t481
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
