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
    var retv134 int32
    retv134 = 7
    return retv134
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    var retv136 string
    retv136 = "seven"
    return retv136
}

func _goml_m_inherent_i_Counter_i_Counter_i_new(start__4 int32, end__5 int32) Counter {
    var retv138 Counter
    var t139 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__4)
    var t140 Counter = Counter{
        current: t139,
        end: end__5,
    }
    retv138 = t140
    return retv138
}

func _goml_m_trait__impl_i_Iterator_i_Counter_i_next(self__6 Counter) Option__int32 {
    var retv142 Option__int32
    var t143 *ref_int32_x = self__6.current
    var current__7 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t143)
    var t146 int32 = self__6.end
    var t147 bool = current__7 < t146
    var jp145 Option__int32
    if t147 {
        var t148 *ref_int32_x = self__6.current
        var t149 int32 = current__7 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t148, t149)
        var t150 Option__int32 = Option__int32_Some{
            _0: current__7,
        }
        jp145 = t150
    } else {
        jp145 = Option__int32_None{}
    }
    retv142 = jp145
    return retv142
}

func main0() struct{} {
    var t152 Token = Token{}
    var t153 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t152)
    println__T_int32(t153)
    var t154 Token = Token{}
    var t155 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t154)
    println__T_string(t155)
    var t156 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t156)
    println__T_int32(converted__8)
    var t157 Any = Any{}
    var t158 string = _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(t157)
    println__T_string(t158)
    var t159 Any = Any{}
    var t160 string = _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(t159)
    println__T_string(t160)
    var t161 Any = Any{}
    var t162 string = _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(t161)
    println__T_string(t162)
    var t163 Counter = _goml_m_inherent_i_Counter_i_Counter_i_new(0, 8)
    var t164 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 MapIterator__int32__int32__Counter = iterator_map__A_int32__B_int32__I_Counter(t163, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t164, p0)
    })
    var t165 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t165, p0)
    })
    var limited__13 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(filtered__12, 3)
    var for_iter115 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(limited__13)
    Loop_loop176:
    for {
        if true {
            var for_next116 Option__int32 = _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(for_iter115)
            switch for_next116.(type) {
            case Option__int32_None:
                break Loop_loop176
            case Option__int32_Some:
                var x117 int32 = for_next116.(Option__int32_Some)._0
                var value__14 int32 = x117
                println__T_int32(value__14)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop176
        }
    }
    var t167 FnIterator__int = _goml_m_range(1, 5)
    var t168 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t167, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t168, p0, p1)
    })
    println__T_int(sum__17)
    var t169 FnIterator__int = _goml_m_range(1, 4)
    var t170 closure_env_main_3 = closure_env_main_3{}
    var t171 MapIterator__int__string__FnIterator__int = _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(t169, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t170, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(t171)
    var for_source121 *_goml_vec_string = texts__19
    var for_limit122 int = vec_len__Vec_6string(for_source121)
    var for_index123 int = 0
    Loop_loop173:
    for {
        var t174 bool = for_index123 < for_limit122
        if t174 {
            var for_item124 string = vec_get__Vec_6string(for_source121, for_index123)
            var t175 int = for_index123 + 1
            for_index123 = t175
            var text__20 string = for_item124
            println__T_string(text__20)
            continue
        } else {
            break Loop_loop173
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv179 *ref_int32_x
    var t180 *ref_int32_x = ref__Ref_5int32(value__207)
    retv179 = t180
    return retv179
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv182 int32
    var t183 int32 = ref_get__Ref_5int32(self__208)
    retv182 = t183
    return retv182
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t190)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    var retv193 int32
    var t194 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(value__2)
    retv193 = t194
    return retv193
}

func _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(self__3 Any) string {
    var retv196 string
    retv196 = "marked"
    return retv196
}

func _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(self__3 Any) string {
    var retv198 string
    retv198 = "marked"
    return retv198
}

func _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(self__3 Any) string {
    var retv200 string
    retv200 = "marked"
    return retv200
}

func iterator_map__A_int32__B_int32__I_Counter(iterator__110 Counter, map_fn__111 func(int32) int32) MapIterator__int32__int32__Counter {
    var retv202 MapIterator__int32__int32__Counter
    var t203 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    retv202 = t203
    return retv202
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__112 MapIterator__int32__int32__Counter, predicate__113 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv205 FilterIterator__int32__MapIterator__int32__int32__Counter
    var t206 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__112,
        predicate: predicate__113,
    }
    retv205 = t206
    return retv205
}

func _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(iterator__114 FilterIterator__int32__MapIterator__int32__int32__Counter, count__115 int) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv208 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var t213 bool = count__115 > 0
    var jp210 int
    if t213 {
        jp210 = count__115
    } else {
        jp210 = 0
    }
    var remaining__116 int = jp210
    var t211 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(remaining__116)
    var t212 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__114,
        remaining: t211,
    }
    retv208 = t212
    return retv208
}

func _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(self__109 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv215 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    retv215 = self__109
    return retv215
}

func _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(self__107 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv217 Option__int32
    var t218 *ref_int_x = self__107.remaining
    var remaining__108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t218)
    var t221 bool = remaining__108 > 0
    var jp220 Option__int32
    if t221 {
        var t222 *ref_int_x = self__107.remaining
        var t223 int = remaining__108 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t222, t223)
        var t224 FilterIterator__int32__MapIterator__int32__int32__Counter = self__107.iterator
        var t225 Option__int32 = _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(t224)
        jp220 = t225
    } else {
        jp220 = Option__int32_None{}
    }
    retv217 = jp220
    return retv217
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__117 FnIterator__int, initial__118 int, combine__119 func(int, int) int) int {
    var retv227 int
    var accumulator__120 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(initial__118)
    Loop_loop230:
    for {
        if true {
            var mtmp26 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(iterator__117)
            switch mtmp26.(type) {
            case Option__int_None:
                break Loop_loop230
            case Option__int_Some:
                var x27 int = mtmp26.(Option__int_Some)._0
                var value__121 int = x27
                var t232 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
                var t233 int = combine__119(t232, value__121)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(accumulator__120, t233)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop230
        }
    }
    var t229 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
    retv227 = t229
    return retv227
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var retv236 FnIterator__int
    var t237 FnIterator__int = __goml_builtin_range(start__222, end__223)
    retv236 = t237
    return retv236
}

func println__T_int(value__1 int) struct{} {
    var t239 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t239)
    return struct{}{}
}

func _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(iterator__122 MapIterator__int__string__FnIterator__int) *_goml_vec_string {
    var retv242 *_goml_vec_string
    var values__123 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    Loop_loop244:
    for {
        if true {
            var mtmp30 Option__string = _goml_m_trait__impl_i_Iterator_h0071ac0c0c2586bdcfed3250d498e716_r____int_i_next(iterator__122)
            switch mtmp30.(type) {
            case Option__string_None:
                break Loop_loop244
            case Option__string_Some:
                var x31 string = mtmp30.(Option__string_Some)._0
                var value__124 string = x31
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__123, value__124)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop244
        }
    }
    retv242 = values__123
    return retv242
}

func _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(iterator__110 FnIterator__int, map_fn__111 func(int) string) MapIterator__int__string__FnIterator__int {
    var retv248 MapIterator__int__string__FnIterator__int
    var t249 MapIterator__int__string__FnIterator__int = MapIterator__int__string__FnIterator__int{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    retv248 = t249
    return retv248
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv251 string
    var t252 string = _goml_runtime_core_int_to_string(self__5)
    retv251 = t252
    return retv251
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv254 string
    var t255 string = _goml_runtime_core_int32_to_string(self__43)
    retv254 = t255
    return retv254
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv257 string
    retv257 = self__38
    return retv257
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv259 *ref_int_x
    var t260 *ref_int_x = ref__Ref_3int(value__207)
    retv259 = t260
    return retv259
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv262 int
    var t263 int = ref_get__Ref_3int(self__208)
    retv262 = t263
    return retv262
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(self__105 FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv267 Option__int32
    Loop_loop269:
    for {
        if true {
            var t270 MapIterator__int32__int32__Counter = self__105.iterator
            var mtmp21 Option__int32 = _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(t270)
            switch mtmp21.(type) {
            case Option__int32_None:
                retv267 = Option__int32_None{}
                return retv267
            case Option__int32_Some:
                var x22 int32 = mtmp21.(Option__int32_Some)._0
                var value__106 int32 = x22
                var t273 func(int32) bool = self__105.predicate
                var t274 bool = t273(value__106)
                if t274 {
                    var t275 Option__int32 = Option__int32_Some{
                        _0: value__106,
                    }
                    retv267 = t275
                    return retv267
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop269
        }
    }
    retv267 = Option__int32_None{}
    return retv267
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv277 Option__int
    var t278 func() Option__int = self__102.next_fn
    var t279 Option__int = t278()
    retv277 = t279
    return retv277
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv281 FnIterator__int
    var current__220 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__218)
    var t282 closure_env_goml_builtin_range_4 = closure_env_goml_builtin_range_4{
        current_0: current__220,
        end_1: end__219,
    }
    var t283 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(t282)
    })
    retv281 = t283
    return retv281
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv285 string
    var t286 string = _goml_runtime_core_int_to_string(self__40)
    retv285 = t286
    return retv285
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv288 *_goml_vec_string
    var t289 *_goml_vec_string = vec_new__Vec_6string()
    retv288 = t289
    return retv288
}

func _goml_m_trait__impl_i_Iterator_h0071ac0c0c2586bdcfed3250d498e716_r____int_i_next(self__103 MapIterator__int__string__FnIterator__int) Option__string {
    var retv291 Option__string
    var t292 FnIterator__int = self__103.iterator
    var mtmp19 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(t292)
    var jp294 Option__string
    switch mtmp19.(type) {
    case Option__int_None:
        jp294 = Option__string_None{}
    case Option__int_Some:
        var x20 int = mtmp19.(Option__int_Some)._0
        var value__104 int = x20
        var t295 func(int) string = self__103.map_fn
        var t296 string = t295(value__104)
        var t297 Option__string = Option__string_Some{
            _0: t296,
        }
        jp294 = t297
    default:
        panic("non-exhaustive match")
    }
    retv291 = jp294
    return retv291
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(self__103 MapIterator__int32__int32__Counter) Option__int32 {
    var retv301 Option__int32
    var t302 Counter = self__103.iterator
    var mtmp19 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Counter_i_next(t302)
    var jp304 Option__int32
    switch mtmp19.(type) {
    case Option__int32_None:
        jp304 = Option__int32_None{}
    case Option__int32_Some:
        var x20 int32 = mtmp19.(Option__int32_Some)._0
        var value__104 int32 = x20
        var t305 func(int32) int32 = self__103.map_fn
        var t306 int32 = t305(value__104)
        var t307 Option__int32 = Option__int32_Some{
            _0: t306,
        }
        jp304 = t307
    default:
        panic("non-exhaustive match")
    }
    retv301 = jp304
    return retv301
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv309 FnIterator__int
    var t310 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv309 = t310
    return retv309
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env128 closure_env_main_0, value__9 int32) int32 {
    var retv324 int32
    var t325 int32 = value__9 * 2
    retv324 = t325
    return retv324
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env129 closure_env_main_1, value__11 int32) bool {
    var retv327 bool
    var t328 bool = value__11 > 4
    retv327 = t328
    return retv327
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env130 closure_env_main_2, total__15 int, value__16 int) int {
    var retv330 int
    var t331 int = total__15 + value__16
    retv330 = t331
    return retv330
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env131 closure_env_main_3, value__18 int) string {
    var retv333 string
    var t334 string = _goml_m_inherent_i_int_i_int_i_to__string(value__18)
    var t335 string = "v" + t334
    retv333 = t335
    return retv333
}

func _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(env132 closure_env_goml_builtin_range_4) Option__int {
    var retv337 Option__int
    var current__220 *ref_int_x = env132.current_0
    var end__219 int = env132.end_1
    var value__221 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__220)
    var t340 bool = value__221 < end__219
    var jp339 Option__int
    if t340 {
        var t341 int = value__221 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__220, t341)
        var t342 Option__int = Option__int_Some{
            _0: value__221,
        }
        jp339 = t342
    } else {
        jp339 = Option__int_None{}
    }
    retv337 = jp339
    return retv337
}

func main() {
    main0()
}
