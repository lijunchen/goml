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
    var t220 Token = Token{}
    var t221 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t220)
    println__T_int32(t221)
    var t222 Token = Token{}
    var t223 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t222)
    println__T_string(t223)
    var t224 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t224)
    println__T_int32(converted__8)
    var t226 string
    t226 = "marked"
    println__T_string(t226)
    var t228 string
    t228 = "marked"
    println__T_string(t228)
    var t230 string
    t230 = "marked"
    var inline486 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline486)
    var t232 Counter
    var inline481 int32 = 0
    var inline482 int32 = 8
    var inline483 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline481)
    var inline484 Counter = Counter{
        current: inline483,
        end: inline482,
    }
    t232 = inline484
    var t233 closure_env_main_0 = closure_env_main_0{}
    var t234 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t233, p0)
    }
    var mapped__10 FnIterator__int32
    var inline477 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4 = closure_env_std_iter_map_A_int32_B_int32_I_Counter_4{
        iterator_0: t232,
        map_fn_1: t234,
    }
    var inline478 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(inline477)
    }
    var inline479 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline478)
    mapped__10 = inline479
    var t235 closure_env_main_1 = closure_env_main_1{}
    var t236 func(int32) bool = func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t235, p0)
    }
    var filtered__12 FnIterator__int32
    var inline473 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5 = closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5{
        iterator_0: mapped__10,
        predicate_1: t236,
    }
    var inline474 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(inline473)
    }
    var inline475 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline474)
    filtered__12 = inline475
    var limited__13 FnIterator__int32
    var inline464 int = 3
    var inline465 bool = inline464 > 0
    var inline467 int
    if inline465 {
        inline467 = inline464
    } else {
        inline467 = 0
    }
    var inline468 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(inline467)
    var inline469 closure_env_std_iter_take_I_FnIterator_int32_6 = closure_env_std_iter_take_I_FnIterator_int32_6{
        remaining_0: inline468,
        iterator_1: filtered__12,
    }
    var inline470 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(inline469)
    }
    var inline471 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline470)
    limited__13 = inline471
    var for_iter179 FnIterator__int32
    for_iter179 = limited__13
    Loop_loop252:
    for {
        var for_next180 Option__int32
        var inline442 func() Option__int32 = for_iter179.next_fn
        var inline443 Option__int32 = inline442()
        for_next180 = inline443
        switch for_next180.(type) {
        case Option__int32_None:
            break Loop_loop252
        case Option__int32_Some:
            var x181 int32 = for_next180.(Option__int32_Some)._0
            var inline439 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x181)
            _goml_runtime_core_string_println(inline439)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t239 FnIterator__int
    var inline459 int = 1
    var inline460 int = 5
    var inline461 FnIterator__int = __goml_builtin_range(inline459, inline460)
    t239 = inline461
    var t240 closure_env_main_2 = closure_env_main_2{}
    var t241 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t240, p0, p1)
    }
    var sum__17 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t239, 0, t241)
    var inline456 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(sum__17)
    _goml_runtime_core_string_println(inline456)
    var t244 FnIterator__int
    var inline452 int = 1
    var inline453 int = 4
    var inline454 FnIterator__int = __goml_builtin_range(inline452, inline453)
    t244 = inline454
    var t245 closure_env_main_3 = closure_env_main_3{}
    var t246 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t245, p0)
    }
    var t247 FnIterator__string
    var inline448 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7 = closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7{
        iterator_0: t244,
        map_fn_1: t246,
    }
    var inline449 func() Option__string = func() Option__string {
        return _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(inline448)
    }
    var inline450 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(inline449)
    t247 = inline450
    var texts__19 *_goml_vec_string = _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(t247)
    var for_limit186 int = vec_len__Vec_6string(texts__19)
    var for_index187 int = 0
    Loop_loop249:
    for {
        var t250 bool = for_index187 < for_limit186
        if t250 {
            var for_item188 string = vec_get__Vec_6string(texts__19, for_index187)
            var t251 int = for_index187 + 1
            for_index187 = t251
            var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item188)
            _goml_runtime_core_string_println(inline445)
            continue
        } else {
            break Loop_loop249
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__257 int32) *ref_int32_x {
    var t256 *ref_int32_x = ref__Ref_5int32(value__257)
    return t256
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__258 *ref_int32_x) int32 {
    var t259 int32 = ref_get__Ref_5int32(self__258)
    return t259
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__259 *ref_int32_x, value__260 int32) struct{} {
    ref_set__Ref_5int32(self__259, value__260)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t263 string
    var inline492 string = _goml_runtime_core_int32_to_string(value__31)
    t263 = inline492
    _goml_runtime_core_string_println(t263)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t266 string
    t266 = value__31
    _goml_runtime_core_string_println(t266)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    return 7
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr304:
    for {
        var mtmp43 Option__int
        var inline504 func() Option__int = iterator__48.next_fn
        var inline505 Option__int = inline504()
        mtmp43 = inline505
        switch mtmp43.(type) {
        case Option__int_None:
            break Loop_loop_expr304
        case Option__int_Some:
            var x44 int = mtmp43.(Option__int_Some)._0
            var t306 int = combine__50(accumulator__51, x44)
            accumulator__51 = t306
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(iterator__53 FnIterator__string) *_goml_vec_string {
    var values__54 *_goml_vec_string
    var inline519 *_goml_vec_string = vec_new__Vec_6string()
    values__54 = inline519
    Loop_loop_expr316:
    for {
        var mtmp47 Option__string
        var inline516 func() Option__string = iterator__53.next_fn
        var inline517 Option__string = inline516()
        mtmp47 = inline517
        switch mtmp47.(type) {
        case Option__string_None:
            break Loop_loop_expr316
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

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t329 string = _goml_runtime_core_int32_to_string(self__72)
    return t329
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__172 func() Option__int32) FnIterator__int32 {
    var t334 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__172,
    }
    return t334
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t337 *ref_int_x = ref__Ref_3int(value__257)
    return t337
}

func __goml_builtin_range(start__268 int, end__269 int) FnIterator__int {
    var current__270 *ref_int_x = ref__Ref_3int(start__268)
    var t349 closure_env_goml_builtin_range_8 = closure_env_goml_builtin_range_8{
        current_0: current__270,
        end_1: end__269,
    }
    var t350 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(t349)
    }
    var inline523 FnIterator__int = FnIterator__int{
        next_fn: t350,
    }
    return inline523
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t354 string = _goml_runtime_core_int_to_string(self__69)
    return t354
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__172 func() Option__string) FnIterator__string {
    var t366 FnIterator__string = FnIterator__string{
        next_fn: next_fn__172,
    }
    return t366
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env192 closure_env_main_0, value__9 int32) int32 {
    var t384 int32 = value__9 * 2
    return t384
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env193 closure_env_main_1, value__11 int32) bool {
    var t387 bool = value__11 > 4
    return t387
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env194 closure_env_main_2, total__15 int, value__16 int) int {
    var t390 int = total__15 + value__16
    return t390
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env195 closure_env_main_3, value__18 int) string {
    var t393 string
    var inline525 string = _goml_runtime_core_int_to_string(value__18)
    t393 = inline525
    var t394 string = "v" + t393
    return t394
}

func _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(env196 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4) Option__int32 {
    var iterator__4 Counter = env196.iterator_0
    var map_fn__5 func(int32) int32 = env196.map_fn_1
    var commute_field560 int32
    var inline527 *ref_int32_x = iterator__4.current
    var inline528 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline527)
    var inline529 int32 = iterator__4.end
    var inline530 bool = inline528 < inline529
    if inline530 {
        var inline531 *ref_int32_x = iterator__4.current
        var inline532 int32 = inline528 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline531, inline532)
        commute_field560 = inline528
        var t399 int32 = map_fn__5(commute_field560)
        var t400 Option__int32 = Option__int32_Some{
            _0: t399,
        }
        return t400
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(env197 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5) Option__int32 {
    var iterator__7 FnIterator__int32 = env197.iterator_0
    var predicate__8 func(int32) bool = env197.predicate_1
    for {
        var mtmp3 Option__int32
        var inline536 func() Option__int32 = iterator__7.next_fn
        var inline537 Option__int32 = inline536()
        mtmp3 = inline537
        switch mtmp3.(type) {
        case Option__int32_None:
            return Option__int32_None{}
        case Option__int32_Some:
            var x4 int32 = mtmp3.(Option__int32_Some)._0
            var t408 bool = predicate__8(x4)
            if t408 {
                var t409 Option__int32 = Option__int32_Some{
                    _0: x4,
                }
                return t409
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
    var t414 int
    var inline546 int = ref_get__Ref_3int(remaining__16)
    t414 = inline546
    var t415 bool = t414 == 0
    if t415 {
        return Option__int32_None{}
    } else {
        var t416 int
        var inline544 int = ref_get__Ref_3int(remaining__16)
        t416 = inline544
        var t417 int = t416 - 1
        ref_set__Ref_3int(remaining__16, t417)
        var inline539 func() Option__int32 = iterator__14.next_fn
        var inline540 Option__int32 = inline539()
        return inline540
    }
}

func _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(env199 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7) Option__string {
    var iterator__4 FnIterator__int = env199.iterator_0
    var map_fn__5 func(int) string = env199.map_fn_1
    var mtmp1 Option__int
    var inline548 func() Option__int = iterator__4.next_fn
    var inline549 Option__int = inline548()
    mtmp1 = inline549
    switch mtmp1.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x2 int = mtmp1.(Option__int_Some)._0
        var t423 string = map_fn__5(x2)
        var t424 Option__string = Option__string_Some{
            _0: t423,
        }
        return t424
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(env200 closure_env_goml_builtin_range_8) Option__int {
    var current__270 *ref_int_x = env200.current_0
    var end__269 int = env200.end_1
    var value__271 int = ref_get__Ref_3int(current__270)
    var t429 bool = value__271 < end__269
    if t429 {
        var t430 int = value__271 + 1
        ref_set__Ref_3int(current__270, t430)
        var t431 Option__int = Option__int_Some{
            _0: value__271,
        }
        return t431
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
