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

type FnIterator__isize struct {
    next_fn func() Option__isize
}

type closure_env_main_0 struct {}

type closure_env_goml_builtin_range_1 struct {
    current_0 *ref_int_x
    end_1 int
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func main0() struct{} {
    var native__0 int = 7
    var small__1 int8 = 8
    var unsigned__2 uint8 = 9
    var t421 string = _goml_runtime_core_int_to_string(native__0)
    var t422 string = _goml_runtime_core_int8_to_string(small__1)
    var t423 string = t421 + t422
    var t424 string = _goml_runtime_core_uint8_to_string(unsigned__2)
    var t425 string = t423 + t424
    var t426 string
    var inline551 string = "abcd"
    var inline552 int = 1
    var inline553 int = 3
    var inline554 bool = string_is_char_boundary(inline551, inline552)
    var inline556 bool
    if inline554 {
        var inline559 bool = string_is_char_boundary(inline551, inline553)
        inline556 = inline559
    } else {
        inline556 = false
    }
    if inline556 {
        var inline557 string = _goml_runtime_core_string_byte_slice(inline551, inline552, inline553)
        t426 = inline557
        var text__3 string = t425 + t426
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t427 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t427)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t428 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t428]
        var t429 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t429]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t430 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t430)
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
        var t432 FnIterator__isize
        var inline547 int = 0
        var inline548 int = 3
        var inline549 FnIterator__isize = __goml_builtin_range(inline547, inline548)
        t432 = inline549
        var t433 closure_env_main_0 = closure_env_main_0{}
        var t434 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t433, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__isize____I__FnIterator_l_isize_r_____T__isize(t432, 0, t434)
        var t435 string
        var inline545 string = _goml_runtime_core_bool_to_string(same__5)
        t435 = inline545
        var t436 string = text__3 + t435
        var t437 int32 = received__10._0
        var t438 string
        var inline543 string = _goml_runtime_core_int32_to_string(t437)
        t438 = inline543
        var t439 string = t436 + t438
        var t440 bool = received__10._1
        var t441 string
        var inline541 string = _goml_runtime_core_bool_to_string(t440)
        t441 = inline541
        var t442 string = t439 + t441
        var t443 string
        var inline539 string = _goml_runtime_core_int_to_string(total__13)
        t443 = inline539
        var t444 string = t442 + t443
        var inline536 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
        _goml_runtime_core_string_println(inline536)
        return struct{}{}
    } else {
        var inline558 string = _goml_runtime_core_string_byte_slice(inline551, -1, -1)
        t426 = inline558
        var text__3 string = t425 + t426
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t427 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t427)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t428 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t428]
        var t429 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t429]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t430 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t430)
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
        var t432 FnIterator__isize
        var inline547 int = 0
        var inline548 int = 3
        var inline549 FnIterator__isize = __goml_builtin_range(inline547, inline548)
        t432 = inline549
        var t433 closure_env_main_0 = closure_env_main_0{}
        var t434 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t433, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__isize____I__FnIterator_l_isize_r_____T__isize(t432, 0, t434)
        var t435 string
        var inline545 string = _goml_runtime_core_bool_to_string(same__5)
        t435 = inline545
        var t436 string = text__3 + t435
        var t437 int32 = received__10._0
        var t438 string
        var inline543 string = _goml_runtime_core_int32_to_string(t437)
        t438 = inline543
        var t439 string = t436 + t438
        var t440 bool = received__10._1
        var t441 string
        var inline541 string = _goml_runtime_core_bool_to_string(t440)
        t441 = inline541
        var t442 string = t439 + t441
        var t443 string
        var inline539 string = _goml_runtime_core_int_to_string(total__13)
        t443 = inline539
        var t444 string = t442 + t443
        var inline536 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
        _goml_runtime_core_string_println(inline536)
        return struct{}{}
    }
}

func _goml_m_std_p_iter_p_fold____A__isize____I__FnIterator_l_isize_r_____T__isize(iterator__48 FnIterator__isize, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr459:
    for {
        var mtmp43 Option__isize
        var inline561 func() Option__isize = iterator__48.next_fn
        var inline562 Option__isize = inline561()
        mtmp43 = inline562
        switch mtmp43._tag {
        case 0:
            break Loop_loop_expr459
        case 1:
            var x44 int = mtmp43._v1_0
            var t461 int = combine__50(accumulator__51, x44)
            accumulator__51 = t461
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t490 bool = index__16 < 0
    var jp482 bool
    if t490 {
        jp482 = true
    } else {
        var t491 int
        var inline570 int = _goml_runtime_core_string_len(value__15)
        t491 = inline570
        var t492 bool = index__16 > t491
        jp482 = t492
    }
    if jp482 {
        return false
    } else {
        var t485 int
        var inline574 int = _goml_runtime_core_string_len(value__15)
        t485 = inline574
        var t486 bool = index__16 == t485
        if t486 {
            return true
        } else {
            var t487 uint8
            var inline572 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t487 = inline572
            var t488_rhs uint8 = 192
            var t488 uint8 = t487 & t488_rhs
            var t489 bool = t488 != 128
            return t489
        }
    }
}

func __goml_builtin_range(start__503 int, end__504 int) FnIterator__isize {
    var current__505 *ref_int_x = ref__Ref_3int(start__503)
    var t499 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__505,
        end_1: end__504,
    }
    var t500 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t499)
    }
    var inline576 FnIterator__isize = FnIterator__isize{
        next_fn: t500,
    }
    return inline576
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env418 closure_env_main_0, sum__11 int, item__12 int) int {
    var t527 int = sum__11 + item__12
    return t527
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env419 closure_env_goml_builtin_range_1) Option__isize {
    var current__505 *ref_int_x = env419.current_0
    var end__504 int = env419.end_1
    var value__506 int = ref_get__Ref_3int(current__505)
    var t532 bool = value__506 < end__504
    if t532 {
        var t533 int = value__506 + 1
        ref_set__Ref_3int(current__505, t533)
        var t534 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__506,
        }
        return t534
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
