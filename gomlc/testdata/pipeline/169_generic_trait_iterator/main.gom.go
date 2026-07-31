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
    var retv178 int32
    retv178 = 7
    return retv178
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    var retv180 string
    retv180 = "seven"
    return retv180
}

func _goml_m_inherent_i_Counter_i_Counter_i_new(start__4 int32, end__5 int32) Counter {
    var retv182 Counter
    var t183 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__4)
    var t184 Counter = Counter{
        current: t183,
        end: end__5,
    }
    retv182 = t184
    return retv182
}

func _goml_m_trait__impl_i_Iterator_i_Counter_i_next(self__6 Counter) Option__int32 {
    var retv186 Option__int32
    var t187 *ref_int32_x = self__6.current
    var current__7 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t187)
    var t190 int32 = self__6.end
    var t191 bool = current__7 < t190
    var jp189 Option__int32
    if t191 {
        var t192 *ref_int32_x = self__6.current
        var t193 int32 = current__7 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t192, t193)
        var t194 Option__int32 = Option__int32_Some{
            _0: current__7,
        }
        jp189 = t194
    } else {
        jp189 = Option__int32_None{}
    }
    retv186 = jp189
    return retv186
}

func main0() struct{} {
    var t196 Token = Token{}
    var t197 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t196)
    println__T_int32(t197)
    var t198 Token = Token{}
    var t199 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t198)
    println__T_string(t199)
    var t200 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t200)
    println__T_int32(converted__8)
    var t201 Any = Any{}
    var t202 string = _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(t201)
    println__T_string(t202)
    var t203 Any = Any{}
    var t204 string = _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(t203)
    println__T_string(t204)
    var t205 Any = Any{}
    var t206 string = _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(t205)
    println__T_string(t206)
    var t207 Counter = _goml_m_inherent_i_Counter_i_Counter_i_new(0, 8)
    var t208 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 MapIterator__int32__int32__Counter = iterator_map__A_int32__B_int32__I_Counter(t207, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t208, p0)
    })
    var t209 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t209, p0)
    })
    var limited__13 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(filtered__12, 3)
    var for_iter159 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(limited__13)
    Loop_loop220:
    for {
        if true {
            var for_next160 Option__int32 = _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(for_iter159)
            switch for_next160.(type) {
            case Option__int32_None:
                break Loop_loop220
            case Option__int32_Some:
                var x161 int32 = for_next160.(Option__int32_Some)._0
                var value__14 int32 = x161
                println__T_int32(value__14)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop220
        }
    }
    var t211 FnIterator__int = _goml_m_range(1, 5)
    var t212 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t211, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t212, p0, p1)
    })
    println__T_int(sum__17)
    var t213 FnIterator__int = _goml_m_range(1, 4)
    var t214 closure_env_main_3 = closure_env_main_3{}
    var t215 MapIterator__int__string__FnIterator__int = _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(t213, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t214, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(t215)
    var for_source165 *_goml_vec_string = texts__19
    var for_limit166 int = vec_len__Vec_6string(for_source165)
    var for_index167 int = 0
    Loop_loop217:
    for {
        var t218 bool = for_index167 < for_limit166
        if t218 {
            var for_item168 string = vec_get__Vec_6string(for_source165, for_index167)
            var t219 int = for_index167 + 1
            for_index167 = t219
            var text__20 string = for_item168
            println__T_string(text__20)
            continue
        } else {
            break Loop_loop217
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv223 *ref_int32_x
    var t224 *ref_int32_x = ref__Ref_5int32(value__207)
    retv223 = t224
    return retv223
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv226 int32
    var t227 int32 = ref_get__Ref_5int32(self__208)
    retv226 = t227
    return retv226
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t231)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t234)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    var retv237 int32
    var t238 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(value__2)
    retv237 = t238
    return retv237
}

func _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(self__3 Any) string {
    var retv240 string
    retv240 = "marked"
    return retv240
}

func _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(self__3 Any) string {
    var retv242 string
    retv242 = "marked"
    return retv242
}

func _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(self__3 Any) string {
    var retv244 string
    retv244 = "marked"
    return retv244
}

func iterator_map__A_int32__B_int32__I_Counter(iterator__110 Counter, map_fn__111 func(int32) int32) MapIterator__int32__int32__Counter {
    var retv246 MapIterator__int32__int32__Counter
    var t247 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    retv246 = t247
    return retv246
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__112 MapIterator__int32__int32__Counter, predicate__113 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv249 FilterIterator__int32__MapIterator__int32__int32__Counter
    var t250 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__112,
        predicate: predicate__113,
    }
    retv249 = t250
    return retv249
}

func _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(iterator__114 FilterIterator__int32__MapIterator__int32__int32__Counter, count__115 int) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv252 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var t257 bool = count__115 > 0
    var jp254 int
    if t257 {
        jp254 = count__115
    } else {
        jp254 = 0
    }
    var remaining__116 int = jp254
    var t255 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(remaining__116)
    var t256 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__114,
        remaining: t255,
    }
    retv252 = t256
    return retv252
}

func _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(self__109 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv259 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    retv259 = self__109
    return retv259
}

func _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(self__107 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv261 Option__int32
    var t262 *ref_int_x = self__107.remaining
    var remaining__108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t262)
    var t265 bool = remaining__108 > 0
    var jp264 Option__int32
    if t265 {
        var t266 *ref_int_x = self__107.remaining
        var t267 int = remaining__108 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t266, t267)
        var t268 FilterIterator__int32__MapIterator__int32__int32__Counter = self__107.iterator
        var t269 Option__int32 = _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(t268)
        jp264 = t269
    } else {
        jp264 = Option__int32_None{}
    }
    retv261 = jp264
    return retv261
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__117 FnIterator__int, initial__118 int, combine__119 func(int, int) int) int {
    var retv271 int
    var accumulator__120 int = initial__118
    Loop_loop273:
    for {
        if true {
            var mtmp28 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(iterator__117)
            switch mtmp28.(type) {
            case Option__int_None:
                break Loop_loop273
            case Option__int_Some:
                var x29 int = mtmp28.(Option__int_Some)._0
                var value__121 int = x29
                var t275 int = combine__119(accumulator__120, value__121)
                accumulator__120 = t275
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop273
        }
    }
    retv271 = accumulator__120
    return retv271
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var retv277 FnIterator__int
    var t278 FnIterator__int = __goml_builtin_range(start__222, end__223)
    retv277 = t278
    return retv277
}

func println__T_int(value__1 int) struct{} {
    var t280 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t280)
    return struct{}{}
}

func _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(iterator__122 MapIterator__int__string__FnIterator__int) *_goml_vec_string {
    var retv283 *_goml_vec_string
    var values__123 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    Loop_loop285:
    for {
        if true {
            var mtmp33 Option__string = _goml_m_trait__impl_i_Iterator_h0071ac0c0c2586bdcfed3250d498e716_r____int_i_next(iterator__122)
            switch mtmp33.(type) {
            case Option__string_None:
                break Loop_loop285
            case Option__string_Some:
                var x34 string = mtmp33.(Option__string_Some)._0
                var value__124 string = x34
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__123, value__124)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop285
        }
    }
    retv283 = values__123
    return retv283
}

func _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(iterator__110 FnIterator__int, map_fn__111 func(int) string) MapIterator__int__string__FnIterator__int {
    var retv289 MapIterator__int__string__FnIterator__int
    var t290 MapIterator__int__string__FnIterator__int = MapIterator__int__string__FnIterator__int{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    retv289 = t290
    return retv289
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv292 string
    var t293 string = _goml_runtime_core_int_to_string(self__5)
    retv292 = t293
    return retv292
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv295 string
    var t296 string = _goml_runtime_core_int32_to_string(self__43)
    retv295 = t296
    return retv295
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv298 string
    retv298 = self__38
    return retv298
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv300 *ref_int_x
    var t301 *ref_int_x = ref__Ref_3int(value__207)
    retv300 = t301
    return retv300
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv303 int
    var t304 int = ref_get__Ref_3int(self__208)
    retv303 = t304
    return retv303
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(self__105 FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv308 Option__int32
    Loop_loop310:
    for {
        if true {
            var t311 MapIterator__int32__int32__Counter = self__105.iterator
            var mtmp23 Option__int32 = _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(t311)
            switch mtmp23.(type) {
            case Option__int32_None:
                retv308 = Option__int32_None{}
                return retv308
            case Option__int32_Some:
                var x24 int32 = mtmp23.(Option__int32_Some)._0
                var value__106 int32 = x24
                var t314 func(int32) bool = self__105.predicate
                var t315 bool = t314(value__106)
                if t315 {
                    var t316 Option__int32 = Option__int32_Some{
                        _0: value__106,
                    }
                    retv308 = t316
                    return retv308
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop310
        }
    }
    retv308 = Option__int32_None{}
    return retv308
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv318 Option__int
    var t319 func() Option__int = self__102.next_fn
    var t320 Option__int = t319()
    retv318 = t320
    return retv318
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv322 FnIterator__int
    var current__220 *ref_int_x = ref__Ref_3int(start__218)
    var t323 closure_env_goml_builtin_range_4 = closure_env_goml_builtin_range_4{
        current_0: current__220,
        end_1: end__219,
    }
    var t324 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(t323)
    })
    retv322 = t324
    return retv322
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv326 string
    var t327 string = _goml_runtime_core_int_to_string(self__40)
    retv326 = t327
    return retv326
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv329 *_goml_vec_string
    var t330 *_goml_vec_string = vec_new__Vec_6string()
    retv329 = t330
    return retv329
}

func _goml_m_trait__impl_i_Iterator_h0071ac0c0c2586bdcfed3250d498e716_r____int_i_next(self__103 MapIterator__int__string__FnIterator__int) Option__string {
    var retv332 Option__string
    var t333 FnIterator__int = self__103.iterator
    var mtmp21 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(t333)
    var jp335 Option__string
    switch mtmp21.(type) {
    case Option__int_None:
        jp335 = Option__string_None{}
    case Option__int_Some:
        var x22 int = mtmp21.(Option__int_Some)._0
        var value__104 int = x22
        var t336 func(int) string = self__103.map_fn
        var t337 string = t336(value__104)
        var t338 Option__string = Option__string_Some{
            _0: t337,
        }
        jp335 = t338
    default:
        panic("non-exhaustive match")
    }
    retv332 = jp335
    return retv332
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(self__103 MapIterator__int32__int32__Counter) Option__int32 {
    var retv342 Option__int32
    var t343 Counter = self__103.iterator
    var mtmp21 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Counter_i_next(t343)
    var jp345 Option__int32
    switch mtmp21.(type) {
    case Option__int32_None:
        jp345 = Option__int32_None{}
    case Option__int32_Some:
        var x22 int32 = mtmp21.(Option__int32_Some)._0
        var value__104 int32 = x22
        var t346 func(int32) int32 = self__103.map_fn
        var t347 int32 = t346(value__104)
        var t348 Option__int32 = Option__int32_Some{
            _0: t347,
        }
        jp345 = t348
    default:
        panic("non-exhaustive match")
    }
    retv342 = jp345
    return retv342
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv350 FnIterator__int
    var t351 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv350 = t351
    return retv350
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env172 closure_env_main_0, value__9 int32) int32 {
    var retv365 int32
    var t366 int32 = value__9 * 2
    retv365 = t366
    return retv365
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env173 closure_env_main_1, value__11 int32) bool {
    var retv368 bool
    var t369 bool = value__11 > 4
    retv368 = t369
    return retv368
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env174 closure_env_main_2, total__15 int, value__16 int) int {
    var retv371 int
    var t372 int = total__15 + value__16
    retv371 = t372
    return retv371
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env175 closure_env_main_3, value__18 int) string {
    var retv374 string
    var t375 string = _goml_m_inherent_i_int_i_int_i_to__string(value__18)
    var t376 string = "v" + t375
    retv374 = t376
    return retv374
}

func _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(env176 closure_env_goml_builtin_range_4) Option__int {
    var retv378 Option__int
    var current__220 *ref_int_x = env176.current_0
    var end__219 int = env176.end_1
    var value__221 int = ref_get__Ref_3int(current__220)
    var t381 bool = value__221 < end__219
    var jp380 Option__int
    if t381 {
        var t382 int = value__221 + 1
        ref_set__Ref_3int(current__220, t382)
        var t383 Option__int = Option__int_Some{
            _0: value__221,
        }
        jp380 = t383
    } else {
        jp380 = Option__int_None{}
    }
    retv378 = jp380
    return retv378
}

func main() {
    main0()
}
