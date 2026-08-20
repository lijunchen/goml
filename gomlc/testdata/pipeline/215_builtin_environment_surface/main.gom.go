package main

import (
    _goml_fmt "fmt"
    _goml_reflect "reflect"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_5int32(a *ref_int32_x, b *ref_int32_x) bool {
    return a == b
}

func ptr_hash__Ref_5int32(reference *ref_int32_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_5int32_4bool struct {
    _0 int32
    _1 bool
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_main_0 struct {}

type closure_env_goml_builtin_range_1 struct {
    current_0 *ref_int_x
    end_1 int
}

type Ordering int32

type Option__int struct {
    _tag int32
    _v1_0 int
}

func main0() struct{} {
    var native__0 int = 7
    var small__1 int8 = 8
    var unsigned__2 uint8 = 9
    var t418 string = _goml_runtime_core_int_to_string(native__0)
    var t419 string = _goml_runtime_core_int8_to_string(small__1)
    var t420 string = t418 + t419
    var t421 string = _goml_runtime_core_uint8_to_string(unsigned__2)
    var t422 string = t420 + t421
    var t423 string
    var inline548 string = "abcd"
    var inline549 int = 1
    var inline550 int = 3
    var inline551 bool = string_is_char_boundary(inline548, inline549)
    var inline553 bool
    if inline551 {
        var inline556 bool = string_is_char_boundary(inline548, inline550)
        inline553 = inline556
    } else {
        inline553 = false
    }
    if inline553 {
        var inline554 string = _goml_runtime_core_string_byte_slice(inline548, inline549, inline550)
        t423 = inline554
        var text__3 string = t422 + t423
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t424 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t424)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t425 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t425]
        var t426 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t426]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t427 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t427)
        var received__10 Tuple2_5int32_4bool = func(p0 chan int32) Tuple2_5int32_4bool {
            var value int32
            var ok bool
            value, ok = <-p0
            return Tuple2_5int32_4bool{
                _0: value,
                _1: ok,
            }
        }(channel__9)
        func(p0 chan int32) struct{} {
            close(p0)
            return struct{}{}
        }(channel__9)
        var t429 FnIterator__int
        var inline544 int = 0
        var inline545 int = 3
        var inline546 FnIterator__int = __goml_builtin_range(inline544, inline545)
        t429 = inline546
        var t430 closure_env_main_0 = closure_env_main_0{}
        var t431 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t430, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t429, 0, t431)
        var t432 string
        var inline542 string = _goml_runtime_core_bool_to_string(same__5)
        t432 = inline542
        var t433 string = text__3 + t432
        var t434 int32 = received__10._0
        var t435 string
        var inline540 string = _goml_runtime_core_int32_to_string(t434)
        t435 = inline540
        var t436 string = t433 + t435
        var t437 bool = received__10._1
        var t438 string
        var inline538 string = _goml_runtime_core_bool_to_string(t437)
        t438 = inline538
        var t439 string = t436 + t438
        var t440 string
        var inline536 string = _goml_runtime_core_int_to_string(total__13)
        t440 = inline536
        var t441 string = t439 + t440
        var inline533 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
        _goml_runtime_core_string_println(inline533)
        return struct{}{}
    } else {
        var inline555 string = _goml_runtime_core_string_byte_slice(inline548, -1, -1)
        t423 = inline555
        var text__3 string = t422 + t423
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t424 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t424)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t425 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t425]
        var t426 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t426]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t427 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t427)
        var received__10 Tuple2_5int32_4bool = func(p0 chan int32) Tuple2_5int32_4bool {
            var value int32
            var ok bool
            value, ok = <-p0
            return Tuple2_5int32_4bool{
                _0: value,
                _1: ok,
            }
        }(channel__9)
        func(p0 chan int32) struct{} {
            close(p0)
            return struct{}{}
        }(channel__9)
        var t429 FnIterator__int
        var inline544 int = 0
        var inline545 int = 3
        var inline546 FnIterator__int = __goml_builtin_range(inline544, inline545)
        t429 = inline546
        var t430 closure_env_main_0 = closure_env_main_0{}
        var t431 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t430, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t429, 0, t431)
        var t432 string
        var inline542 string = _goml_runtime_core_bool_to_string(same__5)
        t432 = inline542
        var t433 string = text__3 + t432
        var t434 int32 = received__10._0
        var t435 string
        var inline540 string = _goml_runtime_core_int32_to_string(t434)
        t435 = inline540
        var t436 string = t433 + t435
        var t437 bool = received__10._1
        var t438 string
        var inline538 string = _goml_runtime_core_bool_to_string(t437)
        t438 = inline538
        var t439 string = t436 + t438
        var t440 string
        var inline536 string = _goml_runtime_core_int_to_string(total__13)
        t440 = inline536
        var t441 string = t439 + t440
        var inline533 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
        _goml_runtime_core_string_println(inline533)
        return struct{}{}
    }
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr456:
    for {
        var mtmp43 Option__int
        var inline558 func() Option__int = iterator__48.next_fn
        var inline559 Option__int = inline558()
        mtmp43 = inline559
        switch mtmp43._tag {
        case 0:
            break Loop_loop_expr456
        case 1:
            var x44 int = mtmp43._v1_0
            var t458 int = combine__50(accumulator__51, x44)
            accumulator__51 = t458
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t487 bool = index__16 < 0
    var jp479 bool
    if t487 {
        jp479 = true
    } else {
        var t488 int
        var inline567 int = _goml_runtime_core_string_len(value__15)
        t488 = inline567
        var t489 bool = index__16 > t488
        jp479 = t489
    }
    if jp479 {
        return false
    } else {
        var t482 int
        var inline571 int = _goml_runtime_core_string_len(value__15)
        t482 = inline571
        var t483 bool = index__16 == t482
        if t483 {
            return true
        } else {
            var t484 uint8
            var inline569 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t484 = inline569
            var t485_rhs uint8 = 192
            var t485 uint8 = t484 & t485_rhs
            var t486 bool = t485 != 128
            return t486
        }
    }
}

func __goml_builtin_range(start__494 int, end__495 int) FnIterator__int {
    var current__496 *ref_int_x = ref__Ref_3int(start__494)
    var t496 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__496,
        end_1: end__495,
    }
    var t497 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t496)
    }
    var inline573 FnIterator__int = FnIterator__int{
        next_fn: t497,
    }
    return inline573
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env415 closure_env_main_0, sum__11 int, item__12 int) int {
    var t524 int = sum__11 + item__12
    return t524
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env416 closure_env_goml_builtin_range_1) Option__int {
    var current__496 *ref_int_x = env416.current_0
    var end__495 int = env416.end_1
    var value__497 int = ref_get__Ref_3int(current__496)
    var t529 bool = value__497 < end__495
    if t529 {
        var t530 int = value__497 + 1
        ref_set__Ref_3int(current__496, t530)
        var t531 Option__int = Option__int{
            _tag: 1,
            _v1_0: value__497,
        }
        return t531
    } else {
        return Option__int{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
