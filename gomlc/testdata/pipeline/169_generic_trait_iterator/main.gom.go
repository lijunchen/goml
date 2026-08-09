package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
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

type Token struct {}

type Any struct {}

type Counter struct {
    current *ref_int32_x
    end int32
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type FnIterator__string struct {
    next_fn func() Option__string
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {}

type closure_env_std_iter_map_A_int32_B_int32_I_Counter_4 struct {
    iterator_0 Counter
    map_fn_1 func(int32) int32
}

type closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5 struct {
    iterator_0 FnIterator__int32
    predicate_1 func(int32) bool
}

type closure_env_std_iter_take_I_FnIterator_int32_6 struct {
    remaining_0 *ref_int_x
    iterator_1 FnIterator__int32
}

type closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7 struct {
    iterator_0 FnIterator__int
    map_fn_1 func(int) string
}

type closure_env_goml_builtin_range_8 struct {
    current_0 *ref_int_x
    end_1 int
}

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(self__0 Token) int32 {
    return 7
}

func main0() struct{} {
    var t221 int32
    t221 = 7
    println__T_int32(t221)
    var t223 string
    t223 = "seven"
    println__T_string(t223)
    var t224 Token = Token{}
    var converted__8 int32
    var inline475 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t224)
    converted__8 = inline475
    var inline472 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(converted__8)
    _goml_runtime_core_string_println(inline472)
    var t226 string
    t226 = "marked"
    var inline468 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline468)
    var t228 string
    t228 = "marked"
    var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline464)
    var t230 string
    t230 = "marked"
    var inline460 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline460)
    var t231 Counter
    var inline455 int32 = 0
    var inline456 int32 = 8
    var inline457 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline455)
    var inline458 Counter = Counter{
        current: inline457,
        end: inline456,
    }
    t231 = inline458
    var t232 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 FnIterator__int32 = _goml_m_std_p_iter_p_map____A__int32____B__int32____I__Counter(t231, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t232, p0)
    })
    var t233 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FnIterator__int32 = _goml_m_std_p_iter_p_filter____I__FnIterator_l_int32_r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t233, p0)
    })
    var limited__13 FnIterator__int32
    var inline447 int = 3
    var inline448 bool = inline447 > 0
    var inline450 int
    if inline448 {
        inline450 = inline447
    } else {
        inline450 = 0
    }
    var inline451 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(inline450)
    var inline452 closure_env_std_iter_take_I_FnIterator_int32_6 = closure_env_std_iter_take_I_FnIterator_int32_6{
        remaining_0: inline451,
        iterator_1: filtered__12,
    }
    var inline453 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(inline452)
    })
    limited__13 = inline453
    var for_iter179 FnIterator__int32
    for_iter179 = limited__13
    Loop_loop244:
    for {
        var for_next180 Option__int32
        var inline429 func() Option__int32 = for_iter179.next_fn
        var inline430 Option__int32 = inline429()
        for_next180 = inline430
        switch for_next180.(type) {
        case Option__int32_None:
            break Loop_loop244
        case Option__int32_Some:
            var x181 int32 = for_next180.(Option__int32_Some)._0
            var inline426 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x181)
            _goml_runtime_core_string_println(inline426)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t235 FnIterator__int
    var inline442 int = 1
    var inline443 int = 5
    var inline444 FnIterator__int = __goml_builtin_range(inline442, inline443)
    t235 = inline444
    var t236 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t235, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t236, p0, p1)
    })
    var inline439 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(sum__17)
    _goml_runtime_core_string_println(inline439)
    var t237 FnIterator__int
    var inline435 int = 1
    var inline436 int = 4
    var inline437 FnIterator__int = __goml_builtin_range(inline435, inline436)
    t237 = inline437
    var t238 closure_env_main_3 = closure_env_main_3{}
    var t239 FnIterator__string = _goml_m_std_p_iter_p_map____A__int____B__string____I__FnIterator_l_int_r_(t237, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t238, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(t239)
    var for_limit186 int = vec_len__Vec_6string(texts__19)
    var for_index187 int = 0
    Loop_loop241:
    for {
        var t242 bool = for_index187 < for_limit186
        if t242 {
            var for_item188 string = vec_get__Vec_6string(texts__19, for_index187)
            var t243 int = for_index187 + 1
            for_index187 = t243
            var inline432 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item188)
            _goml_runtime_core_string_println(inline432)
            continue
        } else {
            break Loop_loop241
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__257 int32) *ref_int32_x {
    var t248 *ref_int32_x = ref__Ref_5int32(value__257)
    return t248
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__258 *ref_int32_x) int32 {
    var t251 int32 = ref_get__Ref_5int32(self__258)
    return t251
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__259 *ref_int32_x, value__260 int32) struct{} {
    ref_set__Ref_5int32(self__259, value__260)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t255 string
    var inline479 string = _goml_runtime_core_int32_to_string(value__31)
    t255 = inline479
    _goml_runtime_core_string_println(t255)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t258 string
    t258 = value__31
    _goml_runtime_core_string_println(t258)
    return struct{}{}
}

func _goml_m_std_p_iter_p_map____A__int32____B__int32____I__Counter(iterator__4 Counter, map_fn__5 func(int32) int32) FnIterator__int32 {
    var t271 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4 = closure_env_std_iter_map_A_int32_B_int32_I_Counter_4{
        iterator_0: iterator__4,
        map_fn_1: map_fn__5,
    }
    var t272 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(t271)
    })
    return t272
}

func _goml_m_std_p_iter_p_filter____I__FnIterator_l_int32_r_____T__int32(iterator__7 FnIterator__int32, predicate__8 func(int32) bool) FnIterator__int32 {
    var t275 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5 = closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5{
        iterator_0: iterator__7,
        predicate_1: predicate__8,
    }
    var t276 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(t275)
    })
    return t276
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr293:
    for {
        var mtmp43 Option__int
        var inline485 func() Option__int = iterator__48.next_fn
        var inline486 Option__int = inline485()
        mtmp43 = inline486
        switch mtmp43.(type) {
        case Option__int_None:
            break Loop_loop_expr293
        case Option__int_Some:
            var x44 int = mtmp43.(Option__int_Some)._0
            var t295 int = combine__50(accumulator__51, x44)
            accumulator__51 = t295
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(iterator__53 FnIterator__string) *_goml_vec_string {
    var values__54 *_goml_vec_string
    var inline499 *_goml_vec_string = vec_new__Vec_6string()
    values__54 = inline499
    Loop_loop_expr305:
    for {
        var mtmp47 Option__string
        var inline496 func() Option__string = iterator__53.next_fn
        var inline497 Option__string = inline496()
        mtmp47 = inline497
        switch mtmp47.(type) {
        case Option__string_None:
            break Loop_loop_expr305
        case Option__string_Some:
            var x48 string = mtmp47.(Option__string_Some)._0
            vec_push__Vec_6string(values__54, x48)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return values__54
}

func _goml_m_std_p_iter_p_map____A__int____B__string____I__FnIterator_l_int_r_(iterator__4 FnIterator__int, map_fn__5 func(int) string) FnIterator__string {
    var t310 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7 = closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7{
        iterator_0: iterator__4,
        map_fn_1: map_fn__5,
    }
    var t311 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(func() Option__string {
        return _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(t310)
    })
    return t311
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t317 string = _goml_runtime_core_int32_to_string(self__72)
    return t317
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__172 func() Option__int32) FnIterator__int32 {
    var t322 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__172,
    }
    return t322
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t325 *ref_int_x = ref__Ref_3int(value__257)
    return t325
}

func __goml_builtin_range(start__268 int, end__269 int) FnIterator__int {
    var current__270 *ref_int_x = ref__Ref_3int(start__268)
    var t337 closure_env_goml_builtin_range_8 = closure_env_goml_builtin_range_8{
        current_0: current__270,
        end_1: end__269,
    }
    var t338 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(t337)
    })
    return t338
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t341 string = _goml_runtime_core_int_to_string(self__69)
    return t341
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__172 func() Option__string) FnIterator__string {
    var t353 FnIterator__string = FnIterator__string{
        next_fn: next_fn__172,
    }
    return t353
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__172 func() Option__int) FnIterator__int {
    var t356 FnIterator__int = FnIterator__int{
        next_fn: next_fn__172,
    }
    return t356
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env192 closure_env_main_0, value__9 int32) int32 {
    var t371 int32 = value__9 * 2
    return t371
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env193 closure_env_main_1, value__11 int32) bool {
    var t374 bool = value__11 > 4
    return t374
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env194 closure_env_main_2, total__15 int, value__16 int) int {
    var t377 int = total__15 + value__16
    return t377
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env195 closure_env_main_3, value__18 int) string {
    var t380 string
    var inline501 string = _goml_runtime_core_int_to_string(value__18)
    t380 = inline501
    var t381 string = "v" + t380
    return t381
}

func _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(env196 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4) Option__int32 {
    var iterator__4 Counter = env196.iterator_0
    var map_fn__5 func(int32) int32 = env196.map_fn_1
    var commute_field536 int32
    var inline503 *ref_int32_x = iterator__4.current
    var inline504 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline503)
    var inline505 int32 = iterator__4.end
    var inline506 bool = inline504 < inline505
    if inline506 {
        var inline507 *ref_int32_x = iterator__4.current
        var inline508 int32 = inline504 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline507, inline508)
        commute_field536 = inline504
        var t386 int32 = map_fn__5(commute_field536)
        var t387 Option__int32 = Option__int32_Some{
            _0: t386,
        }
        return t387
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(env197 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5) Option__int32 {
    var iterator__7 FnIterator__int32 = env197.iterator_0
    var predicate__8 func(int32) bool = env197.predicate_1
    for {
        var mtmp3 Option__int32
        var inline512 func() Option__int32 = iterator__7.next_fn
        var inline513 Option__int32 = inline512()
        mtmp3 = inline513
        switch mtmp3.(type) {
        case Option__int32_None:
            return Option__int32_None{}
        case Option__int32_Some:
            var x4 int32 = mtmp3.(Option__int32_Some)._0
            var t395 bool = predicate__8(x4)
            if t395 {
                var t396 Option__int32 = Option__int32_Some{
                    _0: x4,
                }
                return t396
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(env198 closure_env_std_iter_take_I_FnIterator_int32_6) Option__int32 {
    var remaining__16 *ref_int_x = env198.remaining_0
    var iterator__14 FnIterator__int32 = env198.iterator_1
    var t401 int
    var inline522 int = ref_get__Ref_3int(remaining__16)
    t401 = inline522
    var t402 bool = t401 == 0
    if t402 {
        return Option__int32_None{}
    } else {
        var t403 int
        var inline520 int = ref_get__Ref_3int(remaining__16)
        t403 = inline520
        var t404 int = t403 - 1
        ref_set__Ref_3int(remaining__16, t404)
        var inline515 func() Option__int32 = iterator__14.next_fn
        var inline516 Option__int32 = inline515()
        return inline516
    }
}

func _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(env199 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7) Option__string {
    var iterator__4 FnIterator__int = env199.iterator_0
    var map_fn__5 func(int) string = env199.map_fn_1
    var mtmp1 Option__int
    var inline524 func() Option__int = iterator__4.next_fn
    var inline525 Option__int = inline524()
    mtmp1 = inline525
    switch mtmp1.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x2 int = mtmp1.(Option__int_Some)._0
        var t410 string = map_fn__5(x2)
        var t411 Option__string = Option__string_Some{
            _0: t410,
        }
        return t411
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(env200 closure_env_goml_builtin_range_8) Option__int {
    var current__270 *ref_int_x = env200.current_0
    var end__269 int = env200.end_1
    var value__271 int = ref_get__Ref_3int(current__270)
    var t416 bool = value__271 < end__269
    if t416 {
        var t417 int = value__271 + 1
        ref_set__Ref_3int(current__270, t417)
        var t418 Option__int = Option__int_Some{
            _0: value__271,
        }
        return t418
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
