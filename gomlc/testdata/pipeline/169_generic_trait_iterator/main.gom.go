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

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    return "seven"
}

func main0() struct{} {
    var t235 Token = Token{}
    var t236 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t235)
    println__T_int32(t236)
    var t237 Token = Token{}
    var t238 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t237)
    println__T_string(t238)
    var t239 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t239)
    println__T_int32(converted__8)
    var t241 string
    t241 = "marked"
    println__T_string(t241)
    var t243 string
    t243 = "marked"
    println__T_string(t243)
    var t245 string
    t245 = "marked"
    var inline501 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t245)
    _goml_runtime_core_string_println(inline501)
    var t247 Counter
    var inline496 int32 = 0
    var inline497 int32 = 8
    var inline498 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline496)
    var inline499 Counter = Counter{
        current: inline498,
        end: inline497,
    }
    t247 = inline499
    var t248 closure_env_main_0 = closure_env_main_0{}
    var t249 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t248, p0)
    }
    var mapped__10 FnIterator__int32
    var inline492 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4 = closure_env_std_iter_map_A_int32_B_int32_I_Counter_4{
        iterator_0: t247,
        map_fn_1: t249,
    }
    var inline493 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(inline492)
    }
    var inline494 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline493)
    mapped__10 = inline494
    var t250 closure_env_main_1 = closure_env_main_1{}
    var t251 func(int32) bool = func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t250, p0)
    }
    var filtered__12 FnIterator__int32
    var inline488 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5 = closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5{
        iterator_0: mapped__10,
        predicate_1: t251,
    }
    var inline489 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(inline488)
    }
    var inline490 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline489)
    filtered__12 = inline490
    var limited__13 FnIterator__int32
    var inline479 int = 3
    var inline480 bool = inline479 > 0
    var inline482 int
    if inline480 {
        inline482 = inline479
    } else {
        inline482 = 0
    }
    var inline483 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(inline482)
    var inline484 closure_env_std_iter_take_I_FnIterator_int32_6 = closure_env_std_iter_take_I_FnIterator_int32_6{
        remaining_0: inline483,
        iterator_1: filtered__12,
    }
    var inline485 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(inline484)
    }
    var inline486 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline485)
    limited__13 = inline486
    var for_iter194 FnIterator__int32
    for_iter194 = limited__13
    Loop_loop267:
    for {
        var for_next195 Option__int32
        var inline457 func() Option__int32 = for_iter194.next_fn
        var inline458 Option__int32 = inline457()
        for_next195 = inline458
        switch for_next195.(type) {
        case Option__int32_None:
            break Loop_loop267
        case Option__int32_Some:
            var x196 int32 = for_next195.(Option__int32_Some)._0
            var inline454 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x196)
            _goml_runtime_core_string_println(inline454)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t254 FnIterator__int
    var inline474 int = 1
    var inline475 int = 5
    var inline476 FnIterator__int = __goml_builtin_range(inline474, inline475)
    t254 = inline476
    var t255 closure_env_main_2 = closure_env_main_2{}
    var t256 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t255, p0, p1)
    }
    var sum__17 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t254, 0, t256)
    var inline471 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(sum__17)
    _goml_runtime_core_string_println(inline471)
    var t259 FnIterator__int
    var inline467 int = 1
    var inline468 int = 4
    var inline469 FnIterator__int = __goml_builtin_range(inline467, inline468)
    t259 = inline469
    var t260 closure_env_main_3 = closure_env_main_3{}
    var t261 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t260, p0)
    }
    var t262 FnIterator__string
    var inline463 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7 = closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7{
        iterator_0: t259,
        map_fn_1: t261,
    }
    var inline464 func() Option__string = func() Option__string {
        return _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(inline463)
    }
    var inline465 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(inline464)
    t262 = inline465
    var texts__19 *_goml_vec_string = _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(t262)
    var for_limit201 int = vec_len__Vec_6string(texts__19)
    var for_index202 int = 0
    Loop_loop264:
    for {
        var t265 bool = for_index202 < for_limit201
        if t265 {
            var for_item203 string = vec_get__Vec_6string(texts__19, for_index202)
            var t266 int = for_index202 + 1
            for_index202 = t266
            var inline460 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item203)
            _goml_runtime_core_string_println(inline460)
            continue
        } else {
            break Loop_loop264
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__273 int32) *ref_int32_x {
    var t271 *ref_int32_x = ref__Ref_5int32(value__273)
    return t271
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__274 *ref_int32_x) int32 {
    var t274 int32 = ref_get__Ref_5int32(self__274)
    return t274
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__275 *ref_int32_x, value__276 int32) struct{} {
    ref_set__Ref_5int32(self__275, value__276)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t278 string
    var inline507 string = _goml_runtime_core_int32_to_string(value__1)
    t278 = inline507
    _goml_runtime_core_string_println(t278)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t281 string
    t281 = value__1
    _goml_runtime_core_string_println(t281)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    return 7
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr319:
    for {
        var mtmp43 Option__int
        var inline519 func() Option__int = iterator__48.next_fn
        var inline520 Option__int = inline519()
        mtmp43 = inline520
        switch mtmp43.(type) {
        case Option__int_None:
            break Loop_loop_expr319
        case Option__int_Some:
            var x44 int = mtmp43.(Option__int_Some)._0
            var t321 int = combine__50(accumulator__51, x44)
            accumulator__51 = t321
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(iterator__53 FnIterator__string) *_goml_vec_string {
    var values__54 *_goml_vec_string
    var inline534 *_goml_vec_string = vec_new__Vec_6string()
    values__54 = inline534
    Loop_loop_expr331:
    for {
        var mtmp47 Option__string
        var inline531 func() Option__string = iterator__53.next_fn
        var inline532 Option__string = inline531()
        mtmp47 = inline532
        switch mtmp47.(type) {
        case Option__string_None:
            break Loop_loop_expr331
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

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t344 string = _goml_runtime_core_int32_to_string(self__70)
    return t344
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__170 func() Option__int32) FnIterator__int32 {
    var t349 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__170,
    }
    return t349
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__273 int) *ref_int_x {
    var t352 *ref_int_x = ref__Ref_3int(value__273)
    return t352
}

func __goml_builtin_range(start__336 int, end__337 int) FnIterator__int {
    var current__338 *ref_int_x = ref__Ref_3int(start__336)
    var t364 closure_env_goml_builtin_range_8 = closure_env_goml_builtin_range_8{
        current_0: current__338,
        end_1: end__337,
    }
    var t365 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(t364)
    }
    var inline538 FnIterator__int = FnIterator__int{
        next_fn: t365,
    }
    return inline538
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t369 string = _goml_runtime_core_int_to_string(self__67)
    return t369
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__170 func() Option__string) FnIterator__string {
    var t381 FnIterator__string = FnIterator__string{
        next_fn: next_fn__170,
    }
    return t381
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env207 closure_env_main_0, value__9 int32) int32 {
    var t399 int32 = value__9 * 2
    return t399
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env208 closure_env_main_1, value__11 int32) bool {
    var t402 bool = value__11 > 4
    return t402
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env209 closure_env_main_2, total__15 int, value__16 int) int {
    var t405 int = total__15 + value__16
    return t405
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env210 closure_env_main_3, value__18 int) string {
    var t408 string
    var inline540 string = _goml_runtime_core_int_to_string(value__18)
    t408 = inline540
    var t409 string = "v" + t408
    return t409
}

func _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(env211 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4) Option__int32 {
    var iterator__4 Counter = env211.iterator_0
    var map_fn__5 func(int32) int32 = env211.map_fn_1
    var commute_field575 int32
    var inline542 *ref_int32_x = iterator__4.current
    var inline543 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline542)
    var inline544 int32 = iterator__4.end
    var inline545 bool = inline543 < inline544
    if inline545 {
        var inline546 *ref_int32_x = iterator__4.current
        var inline547 int32 = inline543 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline546, inline547)
        commute_field575 = inline543
        var t414 int32 = map_fn__5(commute_field575)
        var t415 Option__int32 = Option__int32_Some{
            _0: t414,
        }
        return t415
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(env212 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5) Option__int32 {
    var iterator__7 FnIterator__int32 = env212.iterator_0
    var predicate__8 func(int32) bool = env212.predicate_1
    for {
        var mtmp3 Option__int32
        var inline551 func() Option__int32 = iterator__7.next_fn
        var inline552 Option__int32 = inline551()
        mtmp3 = inline552
        switch mtmp3.(type) {
        case Option__int32_None:
            return Option__int32_None{}
        case Option__int32_Some:
            var x4 int32 = mtmp3.(Option__int32_Some)._0
            var t423 bool = predicate__8(x4)
            if t423 {
                var t424 Option__int32 = Option__int32_Some{
                    _0: x4,
                }
                return t424
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(env213 closure_env_std_iter_take_I_FnIterator_int32_6) Option__int32 {
    var remaining__16 *ref_int_x = env213.remaining_0
    var iterator__14 FnIterator__int32 = env213.iterator_1
    var t429 int
    var inline561 int = ref_get__Ref_3int(remaining__16)
    t429 = inline561
    var t430 bool = t429 == 0
    if t430 {
        return Option__int32_None{}
    } else {
        var t431 int
        var inline559 int = ref_get__Ref_3int(remaining__16)
        t431 = inline559
        var t432 int = t431 - 1
        ref_set__Ref_3int(remaining__16, t432)
        var inline554 func() Option__int32 = iterator__14.next_fn
        var inline555 Option__int32 = inline554()
        return inline555
    }
}

func _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(env214 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7) Option__string {
    var iterator__4 FnIterator__int = env214.iterator_0
    var map_fn__5 func(int) string = env214.map_fn_1
    var mtmp1 Option__int
    var inline563 func() Option__int = iterator__4.next_fn
    var inline564 Option__int = inline563()
    mtmp1 = inline564
    switch mtmp1.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x2 int = mtmp1.(Option__int_Some)._0
        var t438 string = map_fn__5(x2)
        var t439 Option__string = Option__string_Some{
            _0: t438,
        }
        return t439
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(env215 closure_env_goml_builtin_range_8) Option__int {
    var current__338 *ref_int_x = env215.current_0
    var end__337 int = env215.end_1
    var value__339 int = ref_get__Ref_3int(current__338)
    var t444 bool = value__339 < end__337
    if t444 {
        var t445 int = value__339 + 1
        ref_set__Ref_3int(current__338, t445)
        var t446 Option__int = Option__int_Some{
            _0: value__339,
        }
        return t446
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
