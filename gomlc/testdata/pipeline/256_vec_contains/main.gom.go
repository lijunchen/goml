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

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type _goml_vec_Pair struct {
    items []Pair
}

func vec_new__Vec_4Pair() *_goml_vec_Pair {
    return &_goml_vec_Pair{
        items: nil,
    }
}

func vec_push__Vec_4Pair(vec *_goml_vec_Pair, elem Pair) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_4Pair(vec *_goml_vec_Pair, index int) Pair {
    return vec.items[index]
}

func vec_len__Vec_4Pair(vec *_goml_vec_Pair) int {
    return int(len(vec.items))
}

type Pair struct {
    left int
    right int
}

type Ordering int32

func main0() struct{} {
    var vec_literal__208 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__208, 1)
    var inline576 int = 2
    vec_push__Vec_3int(vec_literal__208, inline576)
    var inline573 int = 3
    vec_push__Vec_3int(vec_literal__208, inline573)
    var t432 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(vec_literal__208, 2)
    var t433 string
    var inline571 string = _goml_runtime_core_bool_to_string(t432)
    t433 = inline571
    var inline568 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
    _goml_runtime_core_string_println(inline568)
    var t434 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(vec_literal__208, 9)
    var t435 string
    var inline566 string = _goml_runtime_core_bool_to_string(t434)
    t435 = inline566
    var inline563 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline563)
    var vec_literal__330 *_goml_vec_string
    var inline561 *_goml_vec_string = vec_new__Vec_6string()
    vec_literal__330 = inline561
    var inline558 string = "alpha"
    vec_push__Vec_6string(vec_literal__330, inline558)
    var inline555 string = "beta"
    vec_push__Vec_6string(vec_literal__330, inline555)
    var t436 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(vec_literal__330, "beta")
    var t437 string
    var inline553 string = _goml_runtime_core_bool_to_string(t436)
    t437 = inline553
    var inline550 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline550)
    var vec_literal__419 *_goml_vec_Pair
    var inline548 *_goml_vec_Pair = vec_new__Vec_4Pair()
    vec_literal__419 = inline548
    var t438 Pair = Pair{
        left: 1,
        right: 2,
    }
    vec_push__Vec_4Pair(vec_literal__419, t438)
    var t439 Pair = Pair{
        left: 1,
        right: 2,
    }
    var t440 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__Pair(vec_literal__419, t439)
    var t441 string
    var inline544 string = _goml_runtime_core_bool_to_string(t440)
    t441 = inline544
    var inline541 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline541)
    var t442 bool
    var inline538 int = 3
    var inline539 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(vec_literal__208, inline538)
    t442 = inline539
    var t443 string
    var inline536 string = _goml_runtime_core_bool_to_string(t442)
    t443 = inline536
    var inline533 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline533)
    var vec_literal__602 *_goml_vec_string
    var inline531 *_goml_vec_string = vec_new__Vec_6string()
    vec_literal__602 = inline531
    var t444 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(vec_literal__602, "x")
    var t445 string
    var inline529 string = _goml_runtime_core_bool_to_string(t444)
    t445 = inline529
    var inline526 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t445)
    _goml_runtime_core_string_println(inline526)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__185 int, other__186 int) bool {
    var t449 bool = self__185 == other__186
    return t449
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t452 *_goml_vec_int = vec_new__Vec_3int()
    return t452
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__258 *_goml_vec_int, elem__259 int) struct{} {
    vec_push__Vec_3int(self__258, elem__259)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(self__384 *_goml_vec_int, expected__385 int) bool {
    var index__386 int = 0
    Loop_loop461:
    for {
        var t462 int
        var inline582 int = vec_len__Vec_3int(self__384)
        t462 = inline582
        var t463 bool = index__386 < t462
        if t463 {
            var t467 int = vec_get__Vec_3int(self__384, index__386)
            var t468 bool
            var inline580 bool = t467 == expected__385
            t468 = inline580
            if t468 {
                return true
            } else {
                var compound_old359 int = index__386
                var compound_value360 int = 1
                var t465 int = compound_old359 + compound_value360
                index__386 = t465
                continue
            }
        } else {
            break Loop_loop461
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(self__384 *_goml_vec_string, expected__385 string) bool {
    var index__386 int = 0
    Loop_loop480:
    for {
        var t481 int
        var inline586 int = vec_len__Vec_6string(self__384)
        t481 = inline586
        var t482 bool = index__386 < t481
        if t482 {
            var t486 string = vec_get__Vec_6string(self__384, index__386)
            var t487 bool
            var inline584 bool = t486 == expected__385
            t487 = inline584
            if t487 {
                return true
            } else {
                var compound_old359 int = index__386
                var compound_value360 int = 1
                var t484 int = compound_old359 + compound_value360
                index__386 = t484
                continue
            }
        } else {
            break Loop_loop480
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__Pair(self__384 *_goml_vec_Pair, expected__385 Pair) bool {
    var index__386 int = 0
    Loop_loop496:
    for {
        var t497 int
        var inline597 int = vec_len__Vec_4Pair(self__384)
        t497 = inline597
        var t498 bool = index__386 < t497
        if t498 {
            var t502 Pair = vec_get__Vec_4Pair(self__384, index__386)
            var t503 bool
            var inline589 bool
            var inline593 int = t502.left
            var inline594 int = expected__385.left
            var inline595 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline593, inline594)
            inline589 = inline595
            if inline589 {
                var inline590 int = t502.right
                var inline591 int = expected__385.right
                var inline592 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline590, inline591)
                t503 = inline592
                if t503 {
                    return true
                } else {
                    var compound_old359 int = index__386
                    var compound_value360 int = 1
                    var t500 int = compound_old359 + compound_value360
                    index__386 = t500
                    continue
                }
            } else {
                t503 = false
                if t503 {
                    return true
                } else {
                    var compound_old359 int = index__386
                    var compound_value360 int = 1
                    var t500 int = compound_old359 + compound_value360
                    index__386 = t500
                    continue
                }
            }
        } else {
            break Loop_loop496
        }
    }
    return false
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
