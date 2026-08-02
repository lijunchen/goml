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
    var retv181 int32
    retv181 = 7
    return retv181
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    var retv183 string
    retv183 = "seven"
    return retv183
}

func _goml_m_inherent_i_Counter_i_Counter_i_new(start__4 int32, end__5 int32) Counter {
    var retv185 Counter
    var t186 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__4)
    var t187 Counter = Counter{
        current: t186,
        end: end__5,
    }
    retv185 = t187
    return retv185
}

func _goml_m_trait__impl_i_Iterator_i_Counter_i_next(self__6 Counter) Option__int32 {
    var retv189 Option__int32
    var t190 *ref_int32_x = self__6.current
    var current__7 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t190)
    var t193 int32 = self__6.end
    var t194 bool = current__7 < t193
    var jp192 Option__int32
    if t194 {
        var t195 *ref_int32_x = self__6.current
        var t196 int32 = current__7 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t195, t196)
        var t197 Option__int32 = Option__int32_Some{
            _0: current__7,
        }
        jp192 = t197
    } else {
        jp192 = Option__int32_None{}
    }
    retv189 = jp192
    return retv189
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
    println__T_string(t205)
    var t206 Any = Any{}
    var t207 string = _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(t206)
    println__T_string(t207)
    var t208 Any = Any{}
    var t209 string = _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(t208)
    println__T_string(t209)
    var t210 Counter = _goml_m_inherent_i_Counter_i_Counter_i_new(0, 8)
    var t211 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 MapIterator__int32__int32__Counter = iterator_map__A_int32__B_int32__I_Counter(t210, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t211, p0)
    })
    var t212 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t212, p0)
    })
    var limited__13 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(filtered__12, 3)
    var for_iter162 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(limited__13)
    Loop_loop223:
    for {
        if true {
            var for_next163 Option__int32 = _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(for_iter162)
            switch for_next163.(type) {
            case Option__int32_None:
                break Loop_loop223
            case Option__int32_Some:
                var x164 int32 = for_next163.(Option__int32_Some)._0
                var value__14 int32 = x164
                println__T_int32(value__14)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop223
        }
    }
    var t214 FnIterator__int = _goml_m_range(1, 5)
    var t215 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t214, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t215, p0, p1)
    })
    println__T_int(sum__17)
    var t216 FnIterator__int = _goml_m_range(1, 4)
    var t217 closure_env_main_3 = closure_env_main_3{}
    var t218 MapIterator__int__string__FnIterator__int = _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(t216, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t217, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(t218)
    var for_source168 *_goml_vec_string = texts__19
    var for_limit169 int = vec_len__Vec_6string(for_source168)
    var for_index170 int = 0
    Loop_loop220:
    for {
        var t221 bool = for_index170 < for_limit169
        if t221 {
            var for_item171 string = vec_get__Vec_6string(for_source168, for_index170)
            var t222 int = for_index170 + 1
            for_index170 = t222
            var text__20 string = for_item171
            println__T_string(text__20)
            continue
        } else {
            break Loop_loop220
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv226 *ref_int32_x
    var t227 *ref_int32_x = ref__Ref_5int32(value__207)
    retv226 = t227
    return retv226
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv229 int32
    var t230 int32 = ref_get__Ref_5int32(self__208)
    retv229 = t230
    return retv229
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t234 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t234)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t237)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    var retv240 int32
    var t241 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(value__2)
    retv240 = t241
    return retv240
}

func _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(self__3 Any) string {
    var retv243 string
    retv243 = "marked"
    return retv243
}

func _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(self__3 Any) string {
    var retv245 string
    retv245 = "marked"
    return retv245
}

func _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(self__3 Any) string {
    var retv247 string
    retv247 = "marked"
    return retv247
}

func iterator_map__A_int32__B_int32__I_Counter(iterator__110 Counter, map_fn__111 func(int32) int32) MapIterator__int32__int32__Counter {
    var retv249 MapIterator__int32__int32__Counter
    var t250 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    retv249 = t250
    return retv249
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__112 MapIterator__int32__int32__Counter, predicate__113 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv252 FilterIterator__int32__MapIterator__int32__int32__Counter
    var t253 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__112,
        predicate: predicate__113,
    }
    retv252 = t253
    return retv252
}

func _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(iterator__114 FilterIterator__int32__MapIterator__int32__int32__Counter, count__115 int) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv255 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var t260 bool = count__115 > 0
    var jp257 int
    if t260 {
        jp257 = count__115
    } else {
        jp257 = 0
    }
    var remaining__116 int = jp257
    var t258 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(remaining__116)
    var t259 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__114,
        remaining: t258,
    }
    retv255 = t259
    return retv255
}

func _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(self__109 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv262 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    retv262 = self__109
    return retv262
}

func _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(self__107 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv264 Option__int32
    var t265 *ref_int_x = self__107.remaining
    var remaining__108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t265)
    var t268 bool = remaining__108 > 0
    var jp267 Option__int32
    if t268 {
        var t269 *ref_int_x = self__107.remaining
        var t270 int = remaining__108 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t269, t270)
        var t271 FilterIterator__int32__MapIterator__int32__int32__Counter = self__107.iterator
        var t272 Option__int32 = _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(t271)
        jp267 = t272
    } else {
        jp267 = Option__int32_None{}
    }
    retv264 = jp267
    return retv264
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__117 FnIterator__int, initial__118 int, combine__119 func(int, int) int) int {
    var retv274 int
    var accumulator__120 int = initial__118
    Loop_loop_expr276:
    for {
        var mtmp28 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(iterator__117)
        switch mtmp28.(type) {
        case Option__int_None:
            break Loop_loop_expr276
        case Option__int_Some:
            var x29 int = mtmp28.(Option__int_Some)._0
            var value__121 int = x29
            var t278 int = combine__119(accumulator__120, value__121)
            accumulator__120 = t278
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    retv274 = accumulator__120
    return retv274
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var retv280 FnIterator__int
    var t281 FnIterator__int = __goml_builtin_range(start__222, end__223)
    retv280 = t281
    return retv280
}

func println__T_int(value__1 int) struct{} {
    var t283 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t283)
    return struct{}{}
}

func _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(iterator__122 MapIterator__int__string__FnIterator__int) *_goml_vec_string {
    var retv286 *_goml_vec_string
    var vec_literal__10204 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var values__123 *_goml_vec_string = vec_literal__10204
    Loop_loop_expr288:
    for {
        var mtmp33 Option__string = _goml_m_trait__impl_i_Iterator_h0071ac0c0c2586bdcfed3250d498e716_r____int_i_next(iterator__122)
        switch mtmp33.(type) {
        case Option__string_None:
            break Loop_loop_expr288
        case Option__string_Some:
            var x34 string = mtmp33.(Option__string_Some)._0
            var value__124 string = x34
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__123, value__124)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    retv286 = values__123
    return retv286
}

func _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(iterator__110 FnIterator__int, map_fn__111 func(int) string) MapIterator__int__string__FnIterator__int {
    var retv292 MapIterator__int__string__FnIterator__int
    var t293 MapIterator__int__string__FnIterator__int = MapIterator__int__string__FnIterator__int{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    retv292 = t293
    return retv292
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv295 string
    var t296 string = _goml_runtime_core_int_to_string(self__5)
    retv295 = t296
    return retv295
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv298 string
    var t299 string = _goml_runtime_core_int32_to_string(self__43)
    retv298 = t299
    return retv298
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv301 string
    retv301 = self__38
    return retv301
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv303 *ref_int_x
    var t304 *ref_int_x = ref__Ref_3int(value__207)
    retv303 = t304
    return retv303
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv306 int
    var t307 int = ref_get__Ref_3int(self__208)
    retv306 = t307
    return retv306
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(self__105 FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv311 Option__int32
    for {
        var t315 MapIterator__int32__int32__Counter = self__105.iterator
        var mtmp23 Option__int32 = _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(t315)
        switch mtmp23.(type) {
        case Option__int32_None:
            retv311 = Option__int32_None{}
            return retv311
        case Option__int32_Some:
            var x24 int32 = mtmp23.(Option__int32_Some)._0
            var value__106 int32 = x24
            var t318 func(int32) bool = self__105.predicate
            var t319 bool = t318(value__106)
            if t319 {
                var t320 Option__int32 = Option__int32_Some{
                    _0: value__106,
                }
                retv311 = t320
                return retv311
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv322 Option__int
    var t323 func() Option__int = self__102.next_fn
    var t324 Option__int = t323()
    retv322 = t324
    return retv322
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv326 FnIterator__int
    var current__220 *ref_int_x = ref__Ref_3int(start__218)
    var t327 closure_env_goml_builtin_range_4 = closure_env_goml_builtin_range_4{
        current_0: current__220,
        end_1: end__219,
    }
    var t328 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(t327)
    })
    retv326 = t328
    return retv326
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv330 string
    var t331 string = _goml_runtime_core_int_to_string(self__40)
    retv330 = t331
    return retv330
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv333 *_goml_vec_string
    var t334 *_goml_vec_string = vec_new__Vec_6string()
    retv333 = t334
    return retv333
}

func _goml_m_trait__impl_i_Iterator_h0071ac0c0c2586bdcfed3250d498e716_r____int_i_next(self__103 MapIterator__int__string__FnIterator__int) Option__string {
    var retv336 Option__string
    var t337 FnIterator__int = self__103.iterator
    var mtmp21 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(t337)
    var jp339 Option__string
    switch mtmp21.(type) {
    case Option__int_None:
        jp339 = Option__string_None{}
    case Option__int_Some:
        var x22 int = mtmp21.(Option__int_Some)._0
        var value__104 int = x22
        var t340 func(int) string = self__103.map_fn
        var t341 string = t340(value__104)
        var t342 Option__string = Option__string_Some{
            _0: t341,
        }
        jp339 = t342
    default:
        panic("non-exhaustive match")
    }
    retv336 = jp339
    return retv336
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(self__103 MapIterator__int32__int32__Counter) Option__int32 {
    var retv346 Option__int32
    var t347 Counter = self__103.iterator
    var mtmp21 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Counter_i_next(t347)
    var jp349 Option__int32
    switch mtmp21.(type) {
    case Option__int32_None:
        jp349 = Option__int32_None{}
    case Option__int32_Some:
        var x22 int32 = mtmp21.(Option__int32_Some)._0
        var value__104 int32 = x22
        var t350 func(int32) int32 = self__103.map_fn
        var t351 int32 = t350(value__104)
        var t352 Option__int32 = Option__int32_Some{
            _0: t351,
        }
        jp349 = t352
    default:
        panic("non-exhaustive match")
    }
    retv346 = jp349
    return retv346
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv354 FnIterator__int
    var t355 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv354 = t355
    return retv354
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env175 closure_env_main_0, value__9 int32) int32 {
    var retv369 int32
    var t370 int32 = value__9 * 2
    retv369 = t370
    return retv369
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env176 closure_env_main_1, value__11 int32) bool {
    var retv372 bool
    var t373 bool = value__11 > 4
    retv372 = t373
    return retv372
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env177 closure_env_main_2, total__15 int, value__16 int) int {
    var retv375 int
    var t376 int = total__15 + value__16
    retv375 = t376
    return retv375
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env178 closure_env_main_3, value__18 int) string {
    var retv378 string
    var t379 string = _goml_m_inherent_i_int_i_int_i_to__string(value__18)
    var t380 string = "v" + t379
    retv378 = t380
    return retv378
}

func _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(env179 closure_env_goml_builtin_range_4) Option__int {
    var retv382 Option__int
    var current__220 *ref_int_x = env179.current_0
    var end__219 int = env179.end_1
    var value__221 int = ref_get__Ref_3int(current__220)
    var t385 bool = value__221 < end__219
    var jp384 Option__int
    if t385 {
        var t386 int = value__221 + 1
        ref_set__Ref_3int(current__220, t386)
        var t387 Option__int = Option__int_Some{
            _0: value__221,
        }
        jp384 = t387
    } else {
        jp384 = Option__int_None{}
    }
    retv382 = jp384
    return retv382
}

func main() {
    main0()
}
