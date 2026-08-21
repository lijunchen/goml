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
    var t429 [3]int = [3]int{1, 2, 3}
    var values__4 *_goml_vec_int = func(values [3]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [3]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t429)
    var t430 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(values__4, 2)
    var t431 string
    var inline543 string = _goml_runtime_core_bool_to_string(t430)
    t431 = inline543
    var inline540 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t431)
    _goml_runtime_core_string_println(inline540)
    var t432 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(values__4, 9)
    var t433 string
    var inline538 string = _goml_runtime_core_bool_to_string(t432)
    t433 = inline538
    var inline535 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
    _goml_runtime_core_string_println(inline535)
    var t434 [2]string = [2]string{"alpha", "beta"}
    var names__5 *_goml_vec_string = func(values [2]string) *_goml_vec_string {
        var storage struct {
            vector _goml_vec_string
            values [2]string
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t434)
    var t435 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(names__5, "beta")
    var t436 string
    var inline533 string = _goml_runtime_core_bool_to_string(t435)
    t436 = inline533
    var inline530 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline530)
    var t437 Pair = Pair{
        left: 1,
        right: 2,
    }
    var t438 [1]Pair = [1]Pair{t437}
    var pairs__6 *_goml_vec_Pair = func(values [1]Pair) *_goml_vec_Pair {
        var storage struct {
            vector _goml_vec_Pair
            values [1]Pair
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t438)
    var t439 Pair = Pair{
        left: 1,
        right: 2,
    }
    var t440 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__Pair(pairs__6, t439)
    var t441 string
    var inline528 string = _goml_runtime_core_bool_to_string(t440)
    t441 = inline528
    var inline525 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline525)
    var t442 bool
    var inline522 int = 3
    var inline523 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(values__4, inline522)
    t442 = inline523
    var t443 string
    var inline520 string = _goml_runtime_core_bool_to_string(t442)
    t443 = inline520
    var inline517 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline517)
    var t444 [0]string = [0]string{}
    var empty__7 *_goml_vec_string = func(values [0]string) *_goml_vec_string {
        return &_goml_vec_string{
            items: values[0:len(values)],
        }
    }(t444)
    var t445 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(empty__7, "x")
    var t446 string
    var inline515 string = _goml_runtime_core_bool_to_string(t445)
    t446 = inline515
    var inline512 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t446)
    _goml_runtime_core_string_println(inline512)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__185 int, other__186 int) bool {
    var t450 bool = self__185 == other__186
    return t450
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__int(self__384 *_goml_vec_int, expected__385 int) bool {
    var index__386 int = 0
    Loop_loop457:
    for {
        var t458 int
        var inline548 int = vec_len__Vec_3int(self__384)
        t458 = inline548
        var t459 bool = index__386 < t458
        if t459 {
            var t463 int = vec_get__Vec_3int(self__384, index__386)
            var t464 bool
            var inline546 bool = t463 == expected__385
            t464 = inline546
            if t464 {
                return true
            } else {
                var compound_old359 int = index__386
                var compound_value360 int = 1
                var t461 int = compound_old359 + compound_value360
                index__386 = t461
                continue
            }
        } else {
            break Loop_loop457
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(self__384 *_goml_vec_string, expected__385 string) bool {
    var index__386 int = 0
    Loop_loop471:
    for {
        var t472 int
        var inline552 int = vec_len__Vec_6string(self__384)
        t472 = inline552
        var t473 bool = index__386 < t472
        if t473 {
            var t477 string = vec_get__Vec_6string(self__384, index__386)
            var t478 bool
            var inline550 bool = t477 == expected__385
            t478 = inline550
            if t478 {
                return true
            } else {
                var compound_old359 int = index__386
                var compound_value360 int = 1
                var t475 int = compound_old359 + compound_value360
                index__386 = t475
                continue
            }
        } else {
            break Loop_loop471
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__Pair(self__384 *_goml_vec_Pair, expected__385 Pair) bool {
    var index__386 int = 0
    Loop_loop482:
    for {
        var t483 int
        var inline563 int = vec_len__Vec_4Pair(self__384)
        t483 = inline563
        var t484 bool = index__386 < t483
        if t484 {
            var t488 Pair = vec_get__Vec_4Pair(self__384, index__386)
            var t489 bool
            var inline555 bool
            var inline559 int = t488.left
            var inline560 int = expected__385.left
            var inline561 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline559, inline560)
            inline555 = inline561
            if inline555 {
                var inline556 int = t488.right
                var inline557 int = expected__385.right
                var inline558 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline556, inline557)
                t489 = inline558
                if t489 {
                    return true
                } else {
                    var compound_old359 int = index__386
                    var compound_value360 int = 1
                    var t486 int = compound_old359 + compound_value360
                    index__386 = t486
                    continue
                }
            } else {
                t489 = false
                if t489 {
                    return true
                } else {
                    var compound_old359 int = index__386
                    var compound_value360 int = 1
                    var t486 int = compound_old359 + compound_value360
                    index__386 = t486
                    continue
                }
            }
        } else {
            break Loop_loop482
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
