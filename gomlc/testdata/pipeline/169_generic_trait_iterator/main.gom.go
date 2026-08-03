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
    var t185 int32
    t185 = 7
    println__T_int32(t185)
    var t187 string
    t187 = "seven"
    println__T_string(t187)
    var t188 Token = Token{}
    var converted__8 int32
    var inline439 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t188)
    converted__8 = inline439
    var inline436 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(converted__8)
    _goml_runtime_core_string_println(inline436)
    var t190 string
    t190 = "marked"
    var inline432 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline432)
    var t192 string
    t192 = "marked"
    var inline428 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline428)
    var t194 string
    t194 = "marked"
    var inline424 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline424)
    var t195 Counter
    var inline419 int32 = 0
    var inline420 int32 = 8
    var inline421 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline419)
    var inline422 Counter = Counter{
        current: inline421,
        end: inline420,
    }
    t195 = inline422
    var t196 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 FnIterator__int32 = _goml_m_std_p_iter_p_map____A__int32____B__int32____I__Counter(t195, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t196, p0)
    })
    var t197 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FnIterator__int32 = _goml_m_std_p_iter_p_filter____I__FnIterator_l_int32_r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t197, p0)
    })
    var limited__13 FnIterator__int32
    var inline411 int = 3
    var inline412 bool = inline411 > 0
    var inline414 int
    if inline412 {
        inline414 = inline411
    } else {
        inline414 = 0
    }
    var inline415 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(inline414)
    var inline416 closure_env_std_iter_take_I_FnIterator_int32_6 = closure_env_std_iter_take_I_FnIterator_int32_6{
        remaining_0: inline415,
        iterator_1: filtered__12,
    }
    var inline417 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(inline416)
    })
    limited__13 = inline417
    var for_iter143 FnIterator__int32
    for_iter143 = limited__13
    Loop_loop208:
    for {
        var for_next144 Option__int32
        var inline393 func() Option__int32 = for_iter143.next_fn
        var inline394 Option__int32 = inline393()
        for_next144 = inline394
        switch for_next144.(type) {
        case Option__int32_None:
            break Loop_loop208
        case Option__int32_Some:
            var x145 int32 = for_next144.(Option__int32_Some)._0
            var inline390 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x145)
            _goml_runtime_core_string_println(inline390)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t199 FnIterator__int
    var inline406 int = 1
    var inline407 int = 5
    var inline408 FnIterator__int = __goml_builtin_range(inline406, inline407)
    t199 = inline408
    var t200 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t199, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t200, p0, p1)
    })
    var inline403 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(sum__17)
    _goml_runtime_core_string_println(inline403)
    var t201 FnIterator__int
    var inline399 int = 1
    var inline400 int = 4
    var inline401 FnIterator__int = __goml_builtin_range(inline399, inline400)
    t201 = inline401
    var t202 closure_env_main_3 = closure_env_main_3{}
    var t203 FnIterator__string = _goml_m_std_p_iter_p_map____A__int____B__string____I__FnIterator_l_int_r_(t201, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t202, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(t203)
    var for_limit150 int = vec_len__Vec_6string(texts__19)
    var for_index151 int = 0
    Loop_loop205:
    for {
        var t206 bool = for_index151 < for_limit150
        if t206 {
            var for_item152 string = vec_get__Vec_6string(texts__19, for_index151)
            var t207 int = for_index151 + 1
            for_index151 = t207
            var inline396 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item152)
            _goml_runtime_core_string_println(inline396)
            continue
        } else {
            break Loop_loop205
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__215 int32) *ref_int32_x {
    var t212 *ref_int32_x = ref__Ref_5int32(value__215)
    return t212
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__216 *ref_int32_x) int32 {
    var t215 int32 = ref_get__Ref_5int32(self__216)
    return t215
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__217 *ref_int32_x, value__218 int32) struct{} {
    ref_set__Ref_5int32(self__217, value__218)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t219 string
    var inline443 string = _goml_runtime_core_int32_to_string(value__31)
    t219 = inline443
    _goml_runtime_core_string_println(t219)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t222 string
    t222 = value__31
    _goml_runtime_core_string_println(t222)
    return struct{}{}
}

func _goml_m_std_p_iter_p_map____A__int32____B__int32____I__Counter(iterator__4 Counter, map_fn__5 func(int32) int32) FnIterator__int32 {
    var t235 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4 = closure_env_std_iter_map_A_int32_B_int32_I_Counter_4{
        iterator_0: iterator__4,
        map_fn_1: map_fn__5,
    }
    var t236 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(t235)
    })
    return t236
}

func _goml_m_std_p_iter_p_filter____I__FnIterator_l_int32_r_____T__int32(iterator__7 FnIterator__int32, predicate__8 func(int32) bool) FnIterator__int32 {
    var t239 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5 = closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5{
        iterator_0: iterator__7,
        predicate_1: predicate__8,
    }
    var t240 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(t239)
    })
    return t240
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr257:
    for {
        var mtmp43 Option__int
        var inline449 func() Option__int = iterator__48.next_fn
        var inline450 Option__int = inline449()
        mtmp43 = inline450
        switch mtmp43.(type) {
        case Option__int_None:
            break Loop_loop_expr257
        case Option__int_Some:
            var x44 int = mtmp43.(Option__int_Some)._0
            var t259 int = combine__50(accumulator__51, x44)
            accumulator__51 = t259
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(iterator__53 FnIterator__string) *_goml_vec_string {
    var values__54 *_goml_vec_string
    var inline463 *_goml_vec_string = vec_new__Vec_6string()
    values__54 = inline463
    Loop_loop_expr269:
    for {
        var mtmp47 Option__string
        var inline460 func() Option__string = iterator__53.next_fn
        var inline461 Option__string = inline460()
        mtmp47 = inline461
        switch mtmp47.(type) {
        case Option__string_None:
            break Loop_loop_expr269
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
    var t274 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7 = closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7{
        iterator_0: iterator__4,
        map_fn_1: map_fn__5,
    }
    var t275 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(func() Option__string {
        return _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(t274)
    })
    return t275
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t281 string = _goml_runtime_core_int32_to_string(self__72)
    return t281
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__130 func() Option__int32) FnIterator__int32 {
    var t286 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__130,
    }
    return t286
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__215 int) *ref_int_x {
    var t289 *ref_int_x = ref__Ref_3int(value__215)
    return t289
}

func __goml_builtin_range(start__226 int, end__227 int) FnIterator__int {
    var current__228 *ref_int_x = ref__Ref_3int(start__226)
    var t304 closure_env_goml_builtin_range_8 = closure_env_goml_builtin_range_8{
        current_0: current__228,
        end_1: end__227,
    }
    var t305 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(t304)
    })
    return t305
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t308 string = _goml_runtime_core_int_to_string(self__69)
    return t308
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__130 func() Option__string) FnIterator__string {
    var t320 FnIterator__string = FnIterator__string{
        next_fn: next_fn__130,
    }
    return t320
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__130 func() Option__int) FnIterator__int {
    var t323 FnIterator__int = FnIterator__int{
        next_fn: next_fn__130,
    }
    return t323
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env156 closure_env_main_0, value__9 int32) int32 {
    var t335 int32 = value__9 * 2
    return t335
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env157 closure_env_main_1, value__11 int32) bool {
    var t338 bool = value__11 > 4
    return t338
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env158 closure_env_main_2, total__15 int, value__16 int) int {
    var t341 int = total__15 + value__16
    return t341
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env159 closure_env_main_3, value__18 int) string {
    var t344 string
    var inline465 string = _goml_runtime_core_int_to_string(value__18)
    t344 = inline465
    var t345 string = "v" + t344
    return t345
}

func _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(env160 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4) Option__int32 {
    var iterator__4 Counter = env160.iterator_0
    var map_fn__5 func(int32) int32 = env160.map_fn_1
    var commute_field503 int32
    var inline467 *ref_int32_x = iterator__4.current
    var inline468 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline467)
    var inline469 int32 = iterator__4.end
    var inline470 bool = inline468 < inline469
    if inline470 {
        var inline471 *ref_int32_x = iterator__4.current
        var inline472 int32 = inline468 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline471, inline472)
        commute_field503 = inline468
        var t350 int32 = map_fn__5(commute_field503)
        var t351 Option__int32 = Option__int32_Some{
            _0: t350,
        }
        return t351
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(env161 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5) Option__int32 {
    var iterator__7 FnIterator__int32 = env161.iterator_0
    var predicate__8 func(int32) bool = env161.predicate_1
    for {
        var mtmp3 Option__int32
        var inline476 func() Option__int32 = iterator__7.next_fn
        var inline477 Option__int32 = inline476()
        mtmp3 = inline477
        switch mtmp3.(type) {
        case Option__int32_None:
            return Option__int32_None{}
        case Option__int32_Some:
            var x4 int32 = mtmp3.(Option__int32_Some)._0
            var t359 bool = predicate__8(x4)
            if t359 {
                var t360 Option__int32 = Option__int32_Some{
                    _0: x4,
                }
                return t360
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(env162 closure_env_std_iter_take_I_FnIterator_int32_6) Option__int32 {
    var remaining__16 *ref_int_x = env162.remaining_0
    var iterator__14 FnIterator__int32 = env162.iterator_1
    var t365 int
    var inline489 int = ref_get__Ref_3int(remaining__16)
    t365 = inline489
    var t366 bool
    var inline486 int = 0
    var inline487 bool = t365 == inline486
    t366 = inline487
    if t366 {
        return Option__int32_None{}
    } else {
        var t367 int
        var inline484 int = ref_get__Ref_3int(remaining__16)
        t367 = inline484
        var t368 int = t367 - 1
        ref_set__Ref_3int(remaining__16, t368)
        var inline479 func() Option__int32 = iterator__14.next_fn
        var inline480 Option__int32 = inline479()
        return inline480
    }
}

func _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(env163 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7) Option__string {
    var iterator__4 FnIterator__int = env163.iterator_0
    var map_fn__5 func(int) string = env163.map_fn_1
    var mtmp1 Option__int
    var inline491 func() Option__int = iterator__4.next_fn
    var inline492 Option__int = inline491()
    mtmp1 = inline492
    switch mtmp1.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x2 int = mtmp1.(Option__int_Some)._0
        var t374 string = map_fn__5(x2)
        var t375 Option__string = Option__string_Some{
            _0: t374,
        }
        return t375
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(env164 closure_env_goml_builtin_range_8) Option__int {
    var current__228 *ref_int_x = env164.current_0
    var end__227 int = env164.end_1
    var value__229 int = ref_get__Ref_3int(current__228)
    var t380 bool = value__229 < end__227
    if t380 {
        var t381 int = value__229 + 1
        ref_set__Ref_3int(current__228, t381)
        var t382 Option__int = Option__int_Some{
            _0: value__229,
        }
        return t382
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
