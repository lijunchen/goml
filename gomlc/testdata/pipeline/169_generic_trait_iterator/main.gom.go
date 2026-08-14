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
    var t230 Token = Token{}
    var t231 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t230)
    println__T_int32(t231)
    var t232 Token = Token{}
    var t233 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t232)
    println__T_string(t233)
    var t234 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t234)
    println__T_int32(converted__8)
    var t236 string
    t236 = "marked"
    println__T_string(t236)
    var t238 string
    t238 = "marked"
    println__T_string(t238)
    var t240 string
    t240 = "marked"
    var inline496 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t240)
    _goml_runtime_core_string_println(inline496)
    var t242 Counter
    var inline491 int32 = 0
    var inline492 int32 = 8
    var inline493 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline491)
    var inline494 Counter = Counter{
        current: inline493,
        end: inline492,
    }
    t242 = inline494
    var t243 closure_env_main_0 = closure_env_main_0{}
    var t244 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t243, p0)
    }
    var mapped__10 FnIterator__int32
    var inline487 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4 = closure_env_std_iter_map_A_int32_B_int32_I_Counter_4{
        iterator_0: t242,
        map_fn_1: t244,
    }
    var inline488 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(inline487)
    }
    var inline489 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline488)
    mapped__10 = inline489
    var t245 closure_env_main_1 = closure_env_main_1{}
    var t246 func(int32) bool = func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t245, p0)
    }
    var filtered__12 FnIterator__int32
    var inline483 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5 = closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5{
        iterator_0: mapped__10,
        predicate_1: t246,
    }
    var inline484 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(inline483)
    }
    var inline485 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline484)
    filtered__12 = inline485
    var limited__13 FnIterator__int32
    var inline474 int = 3
    var inline475 bool = inline474 > 0
    var inline477 int
    if inline475 {
        inline477 = inline474
    } else {
        inline477 = 0
    }
    var inline478 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(inline477)
    var inline479 closure_env_std_iter_take_I_FnIterator_int32_6 = closure_env_std_iter_take_I_FnIterator_int32_6{
        remaining_0: inline478,
        iterator_1: filtered__12,
    }
    var inline480 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(inline479)
    }
    var inline481 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(inline480)
    limited__13 = inline481
    var for_iter189 FnIterator__int32
    for_iter189 = limited__13
    Loop_loop262:
    for {
        var for_next190 Option__int32
        var inline452 func() Option__int32 = for_iter189.next_fn
        var inline453 Option__int32 = inline452()
        for_next190 = inline453
        switch for_next190.(type) {
        case Option__int32_None:
            break Loop_loop262
        case Option__int32_Some:
            var x191 int32 = for_next190.(Option__int32_Some)._0
            var inline449 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x191)
            _goml_runtime_core_string_println(inline449)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t249 FnIterator__int
    var inline469 int = 1
    var inline470 int = 5
    var inline471 FnIterator__int = __goml_builtin_range(inline469, inline470)
    t249 = inline471
    var t250 closure_env_main_2 = closure_env_main_2{}
    var t251 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t250, p0, p1)
    }
    var sum__17 int = _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(t249, 0, t251)
    var inline466 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(sum__17)
    _goml_runtime_core_string_println(inline466)
    var t254 FnIterator__int
    var inline462 int = 1
    var inline463 int = 4
    var inline464 FnIterator__int = __goml_builtin_range(inline462, inline463)
    t254 = inline464
    var t255 closure_env_main_3 = closure_env_main_3{}
    var t256 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t255, p0)
    }
    var t257 FnIterator__string
    var inline458 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7 = closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7{
        iterator_0: t254,
        map_fn_1: t256,
    }
    var inline459 func() Option__string = func() Option__string {
        return _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(inline458)
    }
    var inline460 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(inline459)
    t257 = inline460
    var texts__19 *_goml_vec_string = _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(t257)
    var for_limit196 int = vec_len__Vec_6string(texts__19)
    var for_index197 int = 0
    Loop_loop259:
    for {
        var t260 bool = for_index197 < for_limit196
        if t260 {
            var for_item198 string = vec_get__Vec_6string(texts__19, for_index197)
            var t261 int = for_index197 + 1
            for_index197 = t261
            var inline455 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item198)
            _goml_runtime_core_string_println(inline455)
            continue
        } else {
            break Loop_loop259
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__270 int32) *ref_int32_x {
    var t266 *ref_int32_x = ref__Ref_5int32(value__270)
    return t266
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__271 *ref_int32_x) int32 {
    var t269 int32 = ref_get__Ref_5int32(self__271)
    return t269
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__272 *ref_int32_x, value__273 int32) struct{} {
    ref_set__Ref_5int32(self__272, value__273)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t273 string
    var inline502 string = _goml_runtime_core_int32_to_string(value__1)
    t273 = inline502
    _goml_runtime_core_string_println(t273)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t276 string
    t276 = value__1
    _goml_runtime_core_string_println(t276)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    return 7
}

func _goml_m_std_p_iter_p_fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__48 FnIterator__int, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr314:
    for {
        var mtmp43 Option__int
        var inline514 func() Option__int = iterator__48.next_fn
        var inline515 Option__int = inline514()
        mtmp43 = inline515
        switch mtmp43.(type) {
        case Option__int_None:
            break Loop_loop_expr314
        case Option__int_Some:
            var x44 int = mtmp43.(Option__int_Some)._0
            var t316 int = combine__50(accumulator__51, x44)
            accumulator__51 = t316
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func _goml_m_std_p_iter_p_collect____I__FnIterator_l_string_r_____T__string(iterator__53 FnIterator__string) *_goml_vec_string {
    var values__54 *_goml_vec_string
    var inline529 *_goml_vec_string = vec_new__Vec_6string()
    values__54 = inline529
    Loop_loop_expr326:
    for {
        var mtmp47 Option__string
        var inline526 func() Option__string = iterator__53.next_fn
        var inline527 Option__string = inline526()
        mtmp47 = inline527
        switch mtmp47.(type) {
        case Option__string_None:
            break Loop_loop_expr326
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
    var t339 string = _goml_runtime_core_int32_to_string(self__70)
    return t339
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__170 func() Option__int32) FnIterator__int32 {
    var t344 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__170,
    }
    return t344
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__270 int) *ref_int_x {
    var t347 *ref_int_x = ref__Ref_3int(value__270)
    return t347
}

func __goml_builtin_range(start__333 int, end__334 int) FnIterator__int {
    var current__335 *ref_int_x = ref__Ref_3int(start__333)
    var t359 closure_env_goml_builtin_range_8 = closure_env_goml_builtin_range_8{
        current_0: current__335,
        end_1: end__334,
    }
    var t360 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(t359)
    }
    var inline533 FnIterator__int = FnIterator__int{
        next_fn: t360,
    }
    return inline533
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t364 string = _goml_runtime_core_int_to_string(self__67)
    return t364
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__170 func() Option__string) FnIterator__string {
    var t376 FnIterator__string = FnIterator__string{
        next_fn: next_fn__170,
    }
    return t376
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env202 closure_env_main_0, value__9 int32) int32 {
    var t394 int32 = value__9 * 2
    return t394
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env203 closure_env_main_1, value__11 int32) bool {
    var t397 bool = value__11 > 4
    return t397
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env204 closure_env_main_2, total__15 int, value__16 int) int {
    var t400 int = total__15 + value__16
    return t400
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env205 closure_env_main_3, value__18 int) string {
    var t403 string
    var inline535 string = _goml_runtime_core_int_to_string(value__18)
    t403 = inline535
    var t404 string = "v" + t403
    return t404
}

func _goml_m_inherent_i_closure__en_h84ca90071873c33e0048783462740cfe_nter__4_i_apply(env206 closure_env_std_iter_map_A_int32_B_int32_I_Counter_4) Option__int32 {
    var iterator__4 Counter = env206.iterator_0
    var map_fn__5 func(int32) int32 = env206.map_fn_1
    var commute_field570 int32
    var inline537 *ref_int32_x = iterator__4.current
    var inline538 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline537)
    var inline539 int32 = iterator__4.end
    var inline540 bool = inline538 < inline539
    if inline540 {
        var inline541 *ref_int32_x = iterator__4.current
        var inline542 int32 = inline538 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline541, inline542)
        commute_field570 = inline538
        var t409 int32 = map_fn__5(commute_field570)
        var t410 Option__int32 = Option__int32_Some{
            _0: t409,
        }
        return t410
    } else {
        return Option__int32_None{}
    }
}

func _goml_m_inherent_i_closure__en_hedf27520c415a1aabc207467607f9d63_nt32__5_i_apply(env207 closure_env_std_iter_filter_I_FnIterator_int32_T_int32_5) Option__int32 {
    var iterator__7 FnIterator__int32 = env207.iterator_0
    var predicate__8 func(int32) bool = env207.predicate_1
    for {
        var mtmp3 Option__int32
        var inline546 func() Option__int32 = iterator__7.next_fn
        var inline547 Option__int32 = inline546()
        mtmp3 = inline547
        switch mtmp3.(type) {
        case Option__int32_None:
            return Option__int32_None{}
        case Option__int32_Some:
            var x4 int32 = mtmp3.(Option__int32_Some)._0
            var t418 bool = predicate__8(x4)
            if t418 {
                var t419 Option__int32 = Option__int32_Some{
                    _0: x4,
                }
                return t419
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_inherent_i_closure__en_heb4c08715b30bde4448bd9026c64bd4f_nt32__6_i_apply(env208 closure_env_std_iter_take_I_FnIterator_int32_6) Option__int32 {
    var remaining__16 *ref_int_x = env208.remaining_0
    var iterator__14 FnIterator__int32 = env208.iterator_1
    var t424 int
    var inline556 int = ref_get__Ref_3int(remaining__16)
    t424 = inline556
    var t425 bool = t424 == 0
    if t425 {
        return Option__int32_None{}
    } else {
        var t426 int
        var inline554 int = ref_get__Ref_3int(remaining__16)
        t426 = inline554
        var t427 int = t426 - 1
        ref_set__Ref_3int(remaining__16, t427)
        var inline549 func() Option__int32 = iterator__14.next_fn
        var inline550 Option__int32 = inline549()
        return inline550
    }
}

func _goml_m_inherent_i_closure__en_hee33584d6eb8d0339494367cef42ffc0__int__7_i_apply(env209 closure_env_std_iter_map_A_int_B_string_I_FnIterator_int_7) Option__string {
    var iterator__4 FnIterator__int = env209.iterator_0
    var map_fn__5 func(int) string = env209.map_fn_1
    var mtmp1 Option__int
    var inline558 func() Option__int = iterator__4.next_fn
    var inline559 Option__int = inline558()
    mtmp1 = inline559
    switch mtmp1.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x2 int = mtmp1.(Option__int_Some)._0
        var t433 string = map_fn__5(x2)
        var t434 Option__string = Option__string_Some{
            _0: t433,
        }
        return t434
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_he266c1d56cf5cac23de741e24d448aa5_ange__8_i_apply(env210 closure_env_goml_builtin_range_8) Option__int {
    var current__335 *ref_int_x = env210.current_0
    var end__334 int = env210.end_1
    var value__336 int = ref_get__Ref_3int(current__335)
    var t439 bool = value__336 < end__334
    if t439 {
        var t440 int = value__336 + 1
        ref_set__Ref_3int(current__335, t440)
        var t441 Option__int = Option__int_Some{
            _0: value__336,
        }
        return t441
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
