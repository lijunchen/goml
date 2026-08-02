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

type MapIterator__int32__int32__Counter struct {
    iterator Counter
    map_fn func(int32) int32
}

type FilterIterator__int32__MapIterator__int32__int32__Counter struct {
    iterator MapIterator__int32__int32__Counter
    predicate func(int32) bool
}

type TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter struct {
    iterator FilterIterator__int32__MapIterator__int32__int32__Counter
    remaining *ref_int_x
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type MapIterator__int__string__FnIterator__int struct {
    iterator FnIterator__int
    map_fn func(int) string
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {}

type closure_env_goml_builtin_range_4 struct {
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

func _goml_m_trait__impl_i_Iterator_i_Counter_i_next(self__6 Counter) Option__int32 {
    var t190 *ref_int32_x = self__6.current
    var current__7 int32
    var inline393 int32 = ref_get__Ref_5int32(t190)
    current__7 = inline393
    var t193 int32 = self__6.end
    var t194 bool = current__7 < t193
    if t194 {
        var t195 *ref_int32_x = self__6.current
        var t196 int32 = current__7 + 1
        ref_set__Ref_5int32(t195, t196)
        var t197 Option__int32 = Option__int32_Some{
            _0: current__7,
        }
        return t197
    } else {
        return Option__int32_None{}
    }
}

func main0() struct{} {
    var t199 Token = Token{}
    var t200 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t199)
    println__T_int32(t200)
    var t201 Token = Token{}
    var t202 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t201)
    println__T_string(t202)
    var t203 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t203)
    println__T_int32(converted__8)
    var t204 Any = Any{}
    var t205 string = _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(t204)
    var inline443 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline443)
    var t207 string
    t207 = "marked"
    var inline439 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline439)
    var t209 string
    t209 = "marked"
    var inline435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline435)
    var t210 Counter
    var inline430 int32 = 0
    var inline431 int32 = 8
    var inline432 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline430)
    var inline433 Counter = Counter{
        current: inline432,
        end: inline431,
    }
    t210 = inline433
    var t211 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 MapIterator__int32__int32__Counter = iterator_map__A_int32__B_int32__I_Counter(t210, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t211, p0)
    })
    var t212 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t212, p0)
    })
    var limited__13 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var inline422 int = 3
    var inline423 bool = inline422 > 0
    var inline425 int
    if inline423 {
        inline425 = inline422
    } else {
        inline425 = 0
    }
    var inline427 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(inline425)
    var inline428 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: filtered__12,
        remaining: inline427,
    }
    limited__13 = inline428
    var for_iter162 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    for_iter162 = limited__13
    Loop_loop223:
    for {
        var for_next163 Option__int32
        var inline398 *ref_int_x = for_iter162.remaining
        var inline399 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline398)
        var inline400 bool = inline399 > 0
        if inline400 {
            var inline401 *ref_int_x = for_iter162.remaining
            var inline402 int = inline399 - 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(inline401, inline402)
            var inline404 FilterIterator__int32__MapIterator__int32__int32__Counter = for_iter162.iterator
            var inline405 Option__int32 = _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(inline404)
            for_next163 = inline405
        } else {
            for_next163 = Option__int32_None{}
        }
        switch for_next163.(type) {
        case Option__int32_None:
            break Loop_loop223
        case Option__int32_Some:
            var x164 int32 = for_next163.(Option__int32_Some)._0
            var inline395 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x164)
            _goml_runtime_core_string_println(inline395)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t214 FnIterator__int
    var inline417 int = 1
    var inline418 int = 5
    var inline419 FnIterator__int = __goml_builtin_range(inline417, inline418)
    t214 = inline419
    var t215 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t214, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t215, p0, p1)
    })
    var inline414 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(sum__17)
    _goml_runtime_core_string_println(inline414)
    var t216 FnIterator__int
    var inline410 int = 1
    var inline411 int = 4
    var inline412 FnIterator__int = __goml_builtin_range(inline410, inline411)
    t216 = inline412
    var t217 closure_env_main_3 = closure_env_main_3{}
    var t218 MapIterator__int__string__FnIterator__int = _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(t216, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t217, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(t218)
    var for_limit169 int = vec_len__Vec_6string(texts__19)
    var for_index170 int = 0
    Loop_loop220:
    for {
        var t221 bool = for_index170 < for_limit169
        if t221 {
            var for_item171 string = vec_get__Vec_6string(texts__19, for_index170)
            var t222 int = for_index170 + 1
            for_index170 = t222
            var inline407 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item171)
            _goml_runtime_core_string_println(inline407)
            continue
        } else {
            break Loop_loop220
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t227 *ref_int32_x = ref__Ref_5int32(value__207)
    return t227
}

func println__T_int32(value__1 int32) struct{} {
    var t234 string
    var inline446 string = _goml_runtime_core_int32_to_string(value__1)
    t234 = inline446
    _goml_runtime_core_string_println(t234)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t237 string
    t237 = value__1
    _goml_runtime_core_string_println(t237)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    return 7
}

func _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(self__3 Any) string {
    return "marked"
}

func iterator_map__A_int32__B_int32__I_Counter(iterator__110 Counter, map_fn__111 func(int32) int32) MapIterator__int32__int32__Counter {
    var t250 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    return t250
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__112 MapIterator__int32__int32__Counter, predicate__113 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var t253 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__112,
        predicate: predicate__113,
    }
    return t253
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__117 FnIterator__int, initial__118 int, combine__119 func(int, int) int) int {
    var accumulator__120 int = initial__118
    Loop_loop_expr276:
    for {
        var mtmp28 Option__int
        var inline456 func() Option__int = iterator__117.next_fn
        var inline457 Option__int = inline456()
        mtmp28 = inline457
        switch mtmp28.(type) {
        case Option__int_None:
            break Loop_loop_expr276
        case Option__int_Some:
            var x29 int = mtmp28.(Option__int_Some)._0
            var t278 int = combine__119(accumulator__120, x29)
            accumulator__120 = t278
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__120
}

func _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(iterator__122 MapIterator__int__string__FnIterator__int) *_goml_vec_string {
    var vec_literal__10204 *_goml_vec_string
    var inline475 *_goml_vec_string = vec_new__Vec_6string()
    vec_literal__10204 = inline475
    Loop_loop_expr288:
    for {
        var mtmp33 Option__string
        var inline467 FnIterator__int = iterator__122.iterator
        var inline468 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(inline467)
        switch inline468.(type) {
        case Option__int_None:
            mtmp33 = Option__string_None{}
        case Option__int_Some:
            var inline469 int = inline468.(Option__int_Some)._0
            var inline471 func(int) string = iterator__122.map_fn
            var inline472 string = inline471(inline469)
            var inline473 Option__string = Option__string_Some{
                _0: inline472,
            }
            mtmp33 = inline473
        default:
            panic("non-exhaustive match")
        }
        switch mtmp33.(type) {
        case Option__string_None:
            break Loop_loop_expr288
        case Option__string_Some:
            var x34 string = mtmp33.(Option__string_Some)._0
            vec_push__Vec_6string(vec_literal__10204, x34)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return vec_literal__10204
}

func _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(iterator__110 FnIterator__int, map_fn__111 func(int) string) MapIterator__int__string__FnIterator__int {
    var t293 MapIterator__int__string__FnIterator__int = MapIterator__int__string__FnIterator__int{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    return t293
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t299 string = _goml_runtime_core_int32_to_string(self__43)
    return t299
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t304 *ref_int_x = ref__Ref_3int(value__207)
    return t304
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t307 int = ref_get__Ref_3int(self__208)
    return t307
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(self__105 FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    for {
        var t315 MapIterator__int32__int32__Counter = self__105.iterator
        var mtmp23 Option__int32
        var inline477 Counter = t315.iterator
        var inline478 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Counter_i_next(inline477)
        switch inline478.(type) {
        case Option__int32_None:
            mtmp23 = Option__int32_None{}
        case Option__int32_Some:
            var inline479 int32 = inline478.(Option__int32_Some)._0
            var inline481 func(int32) int32 = t315.map_fn
            var inline482 int32 = inline481(inline479)
            var inline483 Option__int32 = Option__int32_Some{
                _0: inline482,
            }
            mtmp23 = inline483
        default:
            panic("non-exhaustive match")
        }
        switch mtmp23.(type) {
        case Option__int32_None:
            return Option__int32_None{}
        case Option__int32_Some:
            var x24 int32 = mtmp23.(Option__int32_Some)._0
            var t318 func(int32) bool = self__105.predicate
            var t319 bool = t318(x24)
            if t319 {
                var t320 Option__int32 = Option__int32_Some{
                    _0: x24,
                }
                return t320
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var t323 func() Option__int = self__102.next_fn
    var t324 Option__int = t323()
    return t324
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var current__220 *ref_int_x = ref__Ref_3int(start__218)
    var t327 closure_env_goml_builtin_range_4 = closure_env_goml_builtin_range_4{
        current_0: current__220,
        end_1: end__219,
    }
    var t328 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(t327)
    })
    return t328
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t331 string = _goml_runtime_core_int_to_string(self__40)
    return t331
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var t355 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    return t355
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env175 closure_env_main_0, value__9 int32) int32 {
    var t370 int32 = value__9 * 2
    return t370
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env176 closure_env_main_1, value__11 int32) bool {
    var t373 bool = value__11 > 4
    return t373
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env177 closure_env_main_2, total__15 int, value__16 int) int {
    var t376 int = total__15 + value__16
    return t376
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env178 closure_env_main_3, value__18 int) string {
    var t379 string
    var inline497 string = _goml_runtime_core_int_to_string(value__18)
    t379 = inline497
    var t380 string = "v" + t379
    return t380
}

func _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(env179 closure_env_goml_builtin_range_4) Option__int {
    var current__220 *ref_int_x = env179.current_0
    var end__219 int = env179.end_1
    var value__221 int = ref_get__Ref_3int(current__220)
    var t385 bool = value__221 < end__219
    if t385 {
        var t386 int = value__221 + 1
        ref_set__Ref_3int(current__220, t386)
        var t387 Option__int = Option__int_Some{
            _0: value__221,
        }
        return t387
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
