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

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type _goml_vec_string struct {
    items []string
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
    var t426 [3]int = [3]int{1, 2, 3}
    var values__4 *_goml_vec_int = func(values [3]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [3]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t426)
    var t427 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(values__4, 2)
    var t428 string
    var inline540 string = _goml_runtime_core_bool_to_string(t427)
    t428 = inline540
    var inline537 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline537)
    var t429 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(values__4, 9)
    var t430 string
    var inline535 string = _goml_runtime_core_bool_to_string(t429)
    t430 = inline535
    var inline532 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline532)
    var t431 [2]string = [2]string{"alpha", "beta"}
    var names__5 *_goml_vec_string = func(values [2]string) *_goml_vec_string {
        var storage struct {
            vector _goml_vec_string
            values [2]string
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t431)
    var t432 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(names__5, "beta")
    var t433 string
    var inline530 string = _goml_runtime_core_bool_to_string(t432)
    t433 = inline530
    var inline527 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
    _goml_runtime_core_string_println(inline527)
    var t434 Pair = Pair{
        left: 1,
        right: 2,
    }
    var t435 [1]Pair = [1]Pair{t434}
    var pairs__6 *_goml_vec_Pair = func(values [1]Pair) *_goml_vec_Pair {
        var storage struct {
            vector _goml_vec_Pair
            values [1]Pair
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t435)
    var t436 Pair = Pair{
        left: 1,
        right: 2,
    }
    var t437 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__Pair(pairs__6, t436)
    var t438 string
    var inline525 string = _goml_runtime_core_bool_to_string(t437)
    t438 = inline525
    var inline522 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline522)
    var t439 bool
    var inline519 int = 3
    var inline520 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(values__4, inline519)
    t439 = inline520
    var t440 string
    var inline517 string = _goml_runtime_core_bool_to_string(t439)
    t440 = inline517
    var inline514 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
    _goml_runtime_core_string_println(inline514)
    var t441 [0]string = [0]string{}
    var empty__7 *_goml_vec_string = func(values [0]string) *_goml_vec_string {
        return &_goml_vec_string{
            items: values[0:len(values)],
        }
    }(t441)
    var t442 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(empty__7, "x")
    var t443 string
    var inline512 string = _goml_runtime_core_bool_to_string(t442)
    t443 = inline512
    var inline509 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline509)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__185 int, other__186 int) bool {
    var t447 bool = self__185 == other__186
    return t447
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(self__384 *_goml_vec_int, expected__385 int) bool {
    var index__386 int = 0
    Loop_loop454:
    for {
        var t455 int
        var inline545 int = vec_len__Vec_3int(self__384)
        t455 = inline545
        var t456 bool = index__386 < t455
        if t456 {
            var t460 int = vec_get__Vec_3int(self__384, index__386)
            var t461 bool
            var inline543 bool = t460 == expected__385
            t461 = inline543
            if t461 {
                return true
            } else {
                var compound_old359 int = index__386
                var compound_value360 int = 1
                var t458 int = compound_old359 + compound_value360
                index__386 = t458
                continue
            }
        } else {
            break Loop_loop454
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(self__384 *_goml_vec_string, expected__385 string) bool {
    var index__386 int = 0
    Loop_loop468:
    for {
        var t469 int
        var inline549 int = vec_len__Vec_6string(self__384)
        t469 = inline549
        var t470 bool = index__386 < t469
        if t470 {
            var t474 string = vec_get__Vec_6string(self__384, index__386)
            var t475 bool
            var inline547 bool = t474 == expected__385
            t475 = inline547
            if t475 {
                return true
            } else {
                var compound_old359 int = index__386
                var compound_value360 int = 1
                var t472 int = compound_old359 + compound_value360
                index__386 = t472
                continue
            }
        } else {
            break Loop_loop468
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__Pair(self__384 *_goml_vec_Pair, expected__385 Pair) bool {
    var index__386 int = 0
    Loop_loop479:
    for {
        var t480 int
        var inline560 int = vec_len__Vec_4Pair(self__384)
        t480 = inline560
        var t481 bool = index__386 < t480
        if t481 {
            var t485 Pair = vec_get__Vec_4Pair(self__384, index__386)
            var t486 bool
            var inline552 bool
            var inline556 int = t485.left
            var inline557 int = expected__385.left
            var inline558 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline556, inline557)
            inline552 = inline558
            if inline552 {
                var inline553 int = t485.right
                var inline554 int = expected__385.right
                var inline555 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline553, inline554)
                t486 = inline555
                if t486 {
                    return true
                } else {
                    var compound_old359 int = index__386
                    var compound_value360 int = 1
                    var t483 int = compound_old359 + compound_value360
                    index__386 = t483
                    continue
                }
            } else {
                t486 = false
                if t486 {
                    return true
                } else {
                    var compound_old359 int = index__386
                    var compound_value360 int = 1
                    var t483 int = compound_old359 + compound_value360
                    index__386 = t483
                    continue
                }
            }
        } else {
            break Loop_loop479
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
