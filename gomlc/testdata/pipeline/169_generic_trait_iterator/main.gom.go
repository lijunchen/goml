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
    var t212 *ref_int32_x = self__6.current
    var current__7 int32
    var inline415 int32 = ref_get__Ref_5int32(t212)
    current__7 = inline415
    var t215 int32 = self__6.end
    var t216 bool = current__7 < t215
    if t216 {
        var t217 *ref_int32_x = self__6.current
        var t218 int32 = current__7 + 1
        ref_set__Ref_5int32(t217, t218)
        var t219 Option__int32 = Option__int32_Some{
            _0: current__7,
        }
        return t219
    } else {
        return Option__int32_None{}
    }
}

func main0() struct{} {
    var t221 Token = Token{}
    var t222 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t221)
    println__T_int32(t222)
    var t223 Token = Token{}
    var t224 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t223)
    println__T_string(t224)
    var t225 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t225)
    println__T_int32(converted__8)
    var t226 Any = Any{}
    var t227 string = _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(t226)
    var inline465 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t227)
    _goml_runtime_core_string_println(inline465)
    var t229 string
    t229 = "marked"
    var inline461 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t229)
    _goml_runtime_core_string_println(inline461)
    var t231 string
    t231 = "marked"
    var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t231)
    _goml_runtime_core_string_println(inline457)
    var t232 Counter
    var inline452 int32 = 0
    var inline453 int32 = 8
    var inline454 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(inline452)
    var inline455 Counter = Counter{
        current: inline454,
        end: inline453,
    }
    t232 = inline455
    var t233 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 MapIterator__int32__int32__Counter = iterator_map__A_int32__B_int32__I_Counter(t232, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t233, p0)
    })
    var t234 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t234, p0)
    })
    var limited__13 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var inline444 int = 3
    var inline445 bool = inline444 > 0
    var inline447 int
    if inline445 {
        inline447 = inline444
    } else {
        inline447 = 0
    }
    var inline449 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(inline447)
    var inline450 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: filtered__12,
        remaining: inline449,
    }
    limited__13 = inline450
    var for_iter184 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    for_iter184 = limited__13
    Loop_loop245:
    for {
        var for_next185 Option__int32
        var inline420 *ref_int_x = for_iter184.remaining
        var inline421 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline420)
        var inline422 bool = inline421 > 0
        if inline422 {
            var inline423 *ref_int_x = for_iter184.remaining
            var inline424 int = inline421 - 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(inline423, inline424)
            var inline426 FilterIterator__int32__MapIterator__int32__int32__Counter = for_iter184.iterator
            var inline427 Option__int32 = _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(inline426)
            for_next185 = inline427
        } else {
            for_next185 = Option__int32_None{}
        }
        switch for_next185.(type) {
        case Option__int32_None:
            break Loop_loop245
        case Option__int32_Some:
            var x186 int32 = for_next185.(Option__int32_Some)._0
            var inline417 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x186)
            _goml_runtime_core_string_println(inline417)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t236 FnIterator__int
    var inline439 int = 1
    var inline440 int = 5
    var inline441 FnIterator__int = __goml_builtin_range(inline439, inline440)
    t236 = inline441
    var t237 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t236, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t237, p0, p1)
    })
    var inline436 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(sum__17)
    _goml_runtime_core_string_println(inline436)
    var t238 FnIterator__int
    var inline432 int = 1
    var inline433 int = 4
    var inline434 FnIterator__int = __goml_builtin_range(inline432, inline433)
    t238 = inline434
    var t239 closure_env_main_3 = closure_env_main_3{}
    var t240 MapIterator__int__string__FnIterator__int = _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(t238, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t239, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(t240)
    var for_limit191 int = vec_len__Vec_6string(texts__19)
    var for_index192 int = 0
    Loop_loop242:
    for {
        var t243 bool = for_index192 < for_limit191
        if t243 {
            var for_item193 string = vec_get__Vec_6string(texts__19, for_index192)
            var t244 int = for_index192 + 1
            for_index192 = t244
            var inline429 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item193)
            _goml_runtime_core_string_println(inline429)
            continue
        } else {
            break Loop_loop242
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__236 int32) *ref_int32_x {
    var t249 *ref_int32_x = ref__Ref_5int32(value__236)
    return t249
}

func println__T_int32(value__31 int32) struct{} {
    var t256 string
    var inline468 string = _goml_runtime_core_int32_to_string(value__31)
    t256 = inline468
    _goml_runtime_core_string_println(t256)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t259 string
    t259 = value__31
    _goml_runtime_core_string_println(t259)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    return 7
}

func _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(self__3 Any) string {
    return "marked"
}

func iterator_map__A_int32__B_int32__I_Counter(iterator__139 Counter, map_fn__140 func(int32) int32) MapIterator__int32__int32__Counter {
    var t272 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__139,
        map_fn: map_fn__140,
    }
    return t272
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__141 MapIterator__int32__int32__Counter, predicate__142 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var t275 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__141,
        predicate: predicate__142,
    }
    return t275
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__146 FnIterator__int, initial__147 int, combine__148 func(int, int) int) int {
    var accumulator__149 int = initial__147
    Loop_loop_expr298:
    for {
        var mtmp50 Option__int
        var inline478 func() Option__int = iterator__146.next_fn
        var inline479 Option__int = inline478()
        mtmp50 = inline479
        switch mtmp50.(type) {
        case Option__int_None:
            break Loop_loop_expr298
        case Option__int_Some:
            var x51 int = mtmp50.(Option__int_Some)._0
            var t300 int = combine__148(accumulator__149, x51)
            accumulator__149 = t300
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__149
}

func _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(iterator__151 MapIterator__int__string__FnIterator__int) *_goml_vec_string {
    var vec_literal__14570 *_goml_vec_string
    var inline497 *_goml_vec_string = vec_new__Vec_6string()
    vec_literal__14570 = inline497
    Loop_loop_expr310:
    for {
        var mtmp55 Option__string
        var inline489 FnIterator__int = iterator__151.iterator
        var inline490 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(inline489)
        switch inline490.(type) {
        case Option__int_None:
            mtmp55 = Option__string_None{}
        case Option__int_Some:
            var inline491 int = inline490.(Option__int_Some)._0
            var inline493 func(int) string = iterator__151.map_fn
            var inline494 string = inline493(inline491)
            var inline495 Option__string = Option__string_Some{
                _0: inline494,
            }
            mtmp55 = inline495
        default:
            panic("non-exhaustive match")
        }
        switch mtmp55.(type) {
        case Option__string_None:
            break Loop_loop_expr310
        case Option__string_Some:
            var x56 string = mtmp55.(Option__string_Some)._0
            vec_push__Vec_6string(vec_literal__14570, x56)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return vec_literal__14570
}

func _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(iterator__139 FnIterator__int, map_fn__140 func(int) string) MapIterator__int__string__FnIterator__int {
    var t315 MapIterator__int__string__FnIterator__int = MapIterator__int__string__FnIterator__int{
        iterator: iterator__139,
        map_fn: map_fn__140,
    }
    return t315
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t321 string = _goml_runtime_core_int32_to_string(self__72)
    return t321
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__236 int) *ref_int_x {
    var t326 *ref_int_x = ref__Ref_3int(value__236)
    return t326
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__237 *ref_int_x) int {
    var t329 int = ref_get__Ref_3int(self__237)
    return t329
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__238 *ref_int_x, value__239 int) struct{} {
    ref_set__Ref_3int(self__238, value__239)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(self__134 FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    for {
        var t337 MapIterator__int32__int32__Counter = self__134.iterator
        var mtmp45 Option__int32
        var inline499 Counter = t337.iterator
        var inline500 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Counter_i_next(inline499)
        switch inline500.(type) {
        case Option__int32_None:
            mtmp45 = Option__int32_None{}
        case Option__int32_Some:
            var inline501 int32 = inline500.(Option__int32_Some)._0
            var inline503 func(int32) int32 = t337.map_fn
            var inline504 int32 = inline503(inline501)
            var inline505 Option__int32 = Option__int32_Some{
                _0: inline504,
            }
            mtmp45 = inline505
        default:
            panic("non-exhaustive match")
        }
        switch mtmp45.(type) {
        case Option__int32_None:
            return Option__int32_None{}
        case Option__int32_Some:
            var x46 int32 = mtmp45.(Option__int32_Some)._0
            var t340 func(int32) bool = self__134.predicate
            var t341 bool = t340(x46)
            if t341 {
                var t342 Option__int32 = Option__int32_Some{
                    _0: x46,
                }
                return t342
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__131 FnIterator__int) Option__int {
    var t345 func() Option__int = self__131.next_fn
    var t346 Option__int = t345()
    return t346
}

func __goml_builtin_range(start__247 int, end__248 int) FnIterator__int {
    var current__249 *ref_int_x = ref__Ref_3int(start__247)
    var t349 closure_env_goml_builtin_range_4 = closure_env_goml_builtin_range_4{
        current_0: current__249,
        end_1: end__248,
    }
    var t350 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(t349)
    })
    return t350
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t353 string = _goml_runtime_core_int_to_string(self__69)
    return t353
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__130 func() Option__int) FnIterator__int {
    var t377 FnIterator__int = FnIterator__int{
        next_fn: next_fn__130,
    }
    return t377
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env197 closure_env_main_0, value__9 int32) int32 {
    var t392 int32 = value__9 * 2
    return t392
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env198 closure_env_main_1, value__11 int32) bool {
    var t395 bool = value__11 > 4
    return t395
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env199 closure_env_main_2, total__15 int, value__16 int) int {
    var t398 int = total__15 + value__16
    return t398
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env200 closure_env_main_3, value__18 int) string {
    var t401 string
    var inline519 string = _goml_runtime_core_int_to_string(value__18)
    t401 = inline519
    var t402 string = "v" + t401
    return t402
}

func _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(env201 closure_env_goml_builtin_range_4) Option__int {
    var current__249 *ref_int_x = env201.current_0
    var end__248 int = env201.end_1
    var value__250 int = ref_get__Ref_3int(current__249)
    var t407 bool = value__250 < end__248
    if t407 {
        var t408 int = value__250 + 1
        ref_set__Ref_3int(current__249, t408)
        var t409 Option__int = Option__int_Some{
            _0: value__250,
        }
        return t409
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
