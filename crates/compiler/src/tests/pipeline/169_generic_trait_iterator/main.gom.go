package main

import (
    _goml_fmt "fmt"
)

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

func vec_get__Vec_6string(vec *_goml_vec_string, index int32) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int32 {
    return int32(len(vec.items))
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

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
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
    remaining *ref_int32_x
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type MapIterator__int32__string__FnIterator__int32 struct {
    iterator FnIterator__int32
    map_fn func(int32) string
}

type FnIterator__string struct {
    next_fn func() Option__string
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {}

type closure_env_range_4 struct {
    current_0 *ref_int32_x
    end_1 int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_string_5 struct {
    index_0 *ref_int32_x
    len_1 int32
    self_2 *_goml_vec_string
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
    var retv86 int32
    retv86 = 7
    return retv86
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    var retv88 string
    retv88 = "seven"
    return retv88
}

func _goml_m_inherent_i_Counter_i_Counter_i_new(start__4 int32, end__5 int32) Counter {
    var retv90 Counter
    var t91 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__4)
    var t92 Counter = Counter{
        current: t91,
        end: end__5,
    }
    retv90 = t92
    return retv90
}

func _goml_m_trait__impl_i_Iterator_i_Counter_i_next(self__6 Counter) Option__int32 {
    var retv94 Option__int32
    var t95 *ref_int32_x = self__6.current
    var current__7 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t95)
    var t98 int32 = self__6.end
    var t99 bool = current__7 < t98
    var jp97 Option__int32
    if t99 {
        var t100 *ref_int32_x = self__6.current
        var t101 int32 = current__7 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t100, t101)
        var t102 Option__int32 = Option__int32_Some{
            _0: current__7,
        }
        jp97 = t102
    } else {
        jp97 = Option__int32_None{}
    }
    retv94 = jp97
    return retv94
}

func main0() struct{} {
    var t104 Token = Token{}
    var t105 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t104)
    println__T_int32(t105)
    var t106 Token = Token{}
    var t107 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t106)
    println__T_string(t107)
    var t108 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t108)
    println__T_int32(converted__8)
    var t109 Any = Any{}
    var t110 string = _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(t109)
    println__T_string(t110)
    var t111 Any = Any{}
    var t112 string = _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(t111)
    println__T_string(t112)
    var t113 Any = Any{}
    var t114 string = _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(t113)
    println__T_string(t114)
    var t115 Counter = _goml_m_inherent_i_Counter_i_Counter_i_new(0, 8)
    var t116 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 MapIterator__int32__int32__Counter = iterator_map__A_int32__B_int32__I_Counter(t115, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t116, p0)
    })
    var t117 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t117, p0)
    })
    var limited__13 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(filtered__12, 3)
    var for_iter68 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(limited__13)
    Loop_loop127:
    for {
        if true {
            var for_next69 Option__int32 = _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(for_iter68)
            switch for_next69.(type) {
            case Option__int32_None:
                break Loop_loop127
            case Option__int32_Some:
                var x70 int32 = for_next69.(Option__int32_Some)._0
                var value__14 int32 = x70
                println__T_int32(value__14)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop127
        }
    }
    var t119 FnIterator__int32 = _goml_m_range(1, 5)
    var t120 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int32 = _goml_m_iterator__fold____A__int32____I__FnIterator_l_int32_r_____T__int32(t119, 0, func(p0 int32, p1 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t120, p0, p1)
    })
    println__T_int32(sum__17)
    var t121 FnIterator__int32 = _goml_m_range(1, 4)
    var t122 closure_env_main_3 = closure_env_main_3{}
    var t123 MapIterator__int32__string__FnIterator__int32 = _goml_m_iterator__map____A__int32____B__string____I__FnIterator_l_int32_r_(t121, func(p0 int32) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t122, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_iterator__collect____I_hc63511b064a501b8bab4d8cd45946ed4_r_____T__string(t123)
    var for_iter74 FnIterator__string = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(texts__19)
    Loop_loop125:
    for {
        if true {
            var for_next75 Option__string = _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(for_iter74)
            switch for_next75.(type) {
            case Option__string_None:
                break Loop_loop125
            case Option__string_Some:
                var x76 string = for_next75.(Option__string_Some)._0
                var text__20 string = x76
                println__T_string(text__20)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop125
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv130 *ref_int32_x
    var t131 *ref_int32_x = ref__Ref_5int32(value__204)
    retv130 = t131
    return retv130
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv133 int32
    var t134 int32 = ref_get__Ref_5int32(self__205)
    retv133 = t134
    return retv133
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t138 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t138)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t141 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t141)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    var retv144 int32
    var t145 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(value__2)
    retv144 = t145
    return retv144
}

func _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(self__3 Any) string {
    var retv147 string
    retv147 = "marked"
    return retv147
}

func _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(self__3 Any) string {
    var retv149 string
    retv149 = "marked"
    return retv149
}

func _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(self__3 Any) string {
    var retv151 string
    retv151 = "marked"
    return retv151
}

func iterator_map__A_int32__B_int32__I_Counter(iterator__105 Counter, map_fn__106 func(int32) int32) MapIterator__int32__int32__Counter {
    var retv153 MapIterator__int32__int32__Counter
    var t154 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__105,
        map_fn: map_fn__106,
    }
    retv153 = t154
    return retv153
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__107 MapIterator__int32__int32__Counter, predicate__108 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv156 FilterIterator__int32__MapIterator__int32__int32__Counter
    var t157 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__107,
        predicate: predicate__108,
    }
    retv156 = t157
    return retv156
}

func _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(iterator__109 FilterIterator__int32__MapIterator__int32__int32__Counter, count__110 int32) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv159 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var t164 bool = count__110 > 0
    var jp161 int32
    if t164 {
        jp161 = count__110
    } else {
        jp161 = 0
    }
    var remaining__111 int32 = jp161
    var t162 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(remaining__111)
    var t163 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__109,
        remaining: t162,
    }
    retv159 = t163
    return retv159
}

func _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(self__104 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv166 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    retv166 = self__104
    return retv166
}

func _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(self__102 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv168 Option__int32
    var t169 *ref_int32_x = self__102.remaining
    var remaining__103 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t169)
    var t172 bool = remaining__103 > 0
    var jp171 Option__int32
    if t172 {
        var t173 *ref_int32_x = self__102.remaining
        var t174 int32 = remaining__103 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t173, t174)
        var t175 FilterIterator__int32__MapIterator__int32__int32__Counter = self__102.iterator
        var t176 Option__int32 = _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(t175)
        jp171 = t176
    } else {
        jp171 = Option__int32_None{}
    }
    retv168 = jp171
    return retv168
}

func _goml_m_iterator__fold____A__int32____I__FnIterator_l_int32_r_____T__int32(iterator__112 FnIterator__int32, initial__113 int32, combine__114 func(int32, int32) int32) int32 {
    var retv178 int32
    var accumulator__115 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(initial__113)
    var running__116 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop181:
    for {
        var t182 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__116)
        if t182 {
            var mtmp26 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(iterator__112)
            switch mtmp26.(type) {
            case Option__int32_None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__116, false)
            case Option__int32_Some:
                var x27 int32 = mtmp26.(Option__int32_Some)._0
                var value__117 int32 = x27
                var t185 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(accumulator__115)
                var t186 int32 = combine__114(t185, value__117)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(accumulator__115, t186)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop181
        }
    }
    var t180 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(accumulator__115)
    retv178 = t180
    return retv178
}

func _goml_m_range(start__208 int32, end__209 int32) FnIterator__int32 {
    var retv189 FnIterator__int32
    var current__210 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__208)
    var t190 closure_env_range_4 = closure_env_range_4{
        current_0: current__210,
        end_1: end__209,
    }
    var t191 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__env__range__4_i_closure__env__range__4_i_apply(t190)
    })
    retv189 = t191
    return retv189
}

func _goml_m_iterator__collect____I_hc63511b064a501b8bab4d8cd45946ed4_r_____T__string(iterator__118 MapIterator__int32__string__FnIterator__int32) *_goml_vec_string {
    var retv193 *_goml_vec_string
    var values__119 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var running__120 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop195:
    for {
        var t196 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__120)
        if t196 {
            var mtmp30 Option__string = _goml_m_trait__impl_i_Iterator_had40bfe6daf831512a74e0ce237eceb9____int32_i_next(iterator__118)
            switch mtmp30.(type) {
            case Option__string_None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__120, false)
            case Option__string_Some:
                var x31 string = mtmp30.(Option__string_Some)._0
                var value__121 string = x31
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__119, value__121)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop195
        }
    }
    retv193 = values__119
    return retv193
}

func _goml_m_iterator__map____A__int32____B__string____I__FnIterator_l_int32_r_(iterator__105 FnIterator__int32, map_fn__106 func(int32) string) MapIterator__int32__string__FnIterator__int32 {
    var retv201 MapIterator__int32__string__FnIterator__int32
    var t202 MapIterator__int32__string__FnIterator__int32 = MapIterator__int32__string__FnIterator__int32{
        iterator: iterator__105,
        map_fn: map_fn__106,
    }
    retv201 = t202
    return retv201
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv204 string
    var t205 string = _goml_runtime_core_int32_to_string(self__5)
    retv204 = t205
    return retv204
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(self__180 *_goml_vec_string) FnIterator__string {
    var retv207 FnIterator__string
    var t208 FnIterator__string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__180)
    retv207 = t208
    return retv207
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(self__97 FnIterator__string) Option__string {
    var retv210 Option__string
    var t211 func() Option__string = self__97.next_fn
    var t212 Option__string = t211()
    retv210 = t212
    return retv210
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv214 string
    var t215 string = _goml_runtime_core_int32_to_string(self__41)
    retv214 = t215
    return retv214
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv217 string
    retv217 = self__37
    return retv217
}

func _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(self__100 FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv219 Option__int32
    Loop_loop221:
    for {
        if true {
            var t222 MapIterator__int32__int32__Counter = self__100.iterator
            var mtmp21 Option__int32 = _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(t222)
            switch mtmp21.(type) {
            case Option__int32_None:
                retv219 = Option__int32_None{}
                return retv219
            case Option__int32_Some:
                var x22 int32 = mtmp21.(Option__int32_Some)._0
                var value__101 int32 = x22
                var t225 func(int32) bool = self__100.predicate
                var t226 bool = t225(value__101)
                if t226 {
                    var t227 Option__int32 = Option__int32_Some{
                        _0: value__101,
                    }
                    retv219 = t227
                    return retv219
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop221
        }
    }
    retv219 = Option__int32_None{}
    return retv219
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__204 bool) *ref_bool_x {
    var retv229 *ref_bool_x
    var t230 *ref_bool_x = ref__Ref_4bool(value__204)
    retv229 = t230
    return retv229
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__205 *ref_bool_x) bool {
    var retv232 bool
    var t233 bool = ref_get__Ref_4bool(self__205)
    retv232 = t233
    return retv232
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__97 FnIterator__int32) Option__int32 {
    var retv235 Option__int32
    var t236 func() Option__int32 = self__97.next_fn
    var t237 Option__int32 = t236()
    retv235 = t237
    return retv235
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__206 *ref_bool_x, value__207 bool) struct{} {
    ref_set__Ref_4bool(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__96 func() Option__int32) FnIterator__int32 {
    var retv241 FnIterator__int32
    var t242 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__96,
    }
    retv241 = t242
    return retv241
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv244 *_goml_vec_string
    var t245 *_goml_vec_string = vec_new__Vec_6string()
    retv244 = t245
    return retv244
}

func _goml_m_trait__impl_i_Iterator_had40bfe6daf831512a74e0ce237eceb9____int32_i_next(self__98 MapIterator__int32__string__FnIterator__int32) Option__string {
    var retv247 Option__string
    var t248 FnIterator__int32 = self__98.iterator
    var mtmp19 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(t248)
    var jp250 Option__string
    switch mtmp19.(type) {
    case Option__int32_None:
        jp250 = Option__string_None{}
    case Option__int32_Some:
        var x20 int32 = mtmp19.(Option__int32_Some)._0
        var value__99 int32 = x20
        var t251 func(int32) string = self__98.map_fn
        var t252 string = t251(value__99)
        var t253 Option__string = Option__string_Some{
            _0: t252,
        }
        jp250 = t253
    default:
        panic("non-exhaustive match")
    }
    retv247 = jp250
    return retv247
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__123 *_goml_vec_string, elem__124 string) struct{} {
    vec_push__Vec_6string(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__175 *_goml_vec_string) FnIterator__string {
    var retv257 FnIterator__string
    var index__176 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__177 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__175)
    var t258 closure_env_inherent_Vec_Vec_T_iter_T_string_5 = closure_env_inherent_Vec_Vec_T_iter_T_string_5{
        index_0: index__176,
        len_1: len__177,
        self_2: self__175,
    }
    var t259 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(func() Option__string {
        return _goml_m_inherent_i_closure__en_h08b6bd77817c533c5e285632edcbf64d_ring__5_i_apply(t258)
    })
    retv257 = t259
    return retv257
}

func _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(self__98 MapIterator__int32__int32__Counter) Option__int32 {
    var retv261 Option__int32
    var t262 Counter = self__98.iterator
    var mtmp19 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Counter_i_next(t262)
    var jp264 Option__int32
    switch mtmp19.(type) {
    case Option__int32_None:
        jp264 = Option__int32_None{}
    case Option__int32_Some:
        var x20 int32 = mtmp19.(Option__int32_Some)._0
        var value__99 int32 = x20
        var t265 func(int32) int32 = self__98.map_fn
        var t266 int32 = t265(value__99)
        var t267 Option__int32 = Option__int32_Some{
            _0: t266,
        }
        jp264 = t267
    default:
        panic("non-exhaustive match")
    }
    retv261 = jp264
    return retv261
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__134 *_goml_vec_string) int32 {
    var retv269 int32
    var t270 int32 = vec_len__Vec_6string(self__134)
    retv269 = t270
    return retv269
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__129 *_goml_vec_string, index__130 int32) string {
    var retv272 string
    var t273 string = vec_get__Vec_6string(self__129, index__130)
    retv272 = t273
    return retv272
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__96 func() Option__string) FnIterator__string {
    var retv275 FnIterator__string
    var t276 FnIterator__string = FnIterator__string{
        next_fn: next_fn__96,
    }
    retv275 = t276
    return retv275
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env79 closure_env_main_0, value__9 int32) int32 {
    var retv291 int32
    var t292 int32 = value__9 * 2
    retv291 = t292
    return retv291
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env80 closure_env_main_1, value__11 int32) bool {
    var retv294 bool
    var t295 bool = value__11 > 4
    retv294 = t295
    return retv294
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env81 closure_env_main_2, total__15 int32, value__16 int32) int32 {
    var retv297 int32
    var t298 int32 = total__15 + value__16
    retv297 = t298
    return retv297
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env82 closure_env_main_3, value__18 int32) string {
    var retv300 string
    var t301 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__18)
    var t302 string = "v" + t301
    retv300 = t302
    return retv300
}

func _goml_m_inherent_i_closure__env__range__4_i_closure__env__range__4_i_apply(env83 closure_env_range_4) Option__int32 {
    var retv304 Option__int32
    var current__210 *ref_int32_x = env83.current_0
    var end__209 int32 = env83.end_1
    var value__211 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__210)
    var t307 bool = value__211 < end__209
    var jp306 Option__int32
    if t307 {
        var t308 int32 = value__211 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__210, t308)
        var t309 Option__int32 = Option__int32_Some{
            _0: value__211,
        }
        jp306 = t309
    } else {
        jp306 = Option__int32_None{}
    }
    retv304 = jp306
    return retv304
}

func _goml_m_inherent_i_closure__en_h08b6bd77817c533c5e285632edcbf64d_ring__5_i_apply(env84 closure_env_inherent_Vec_Vec_T_iter_T_string_5) Option__string {
    var retv311 Option__string
    var index__176 *ref_int32_x = env84.index_0
    var len__177 int32 = env84.len_1
    var self__175 *_goml_vec_string = env84.self_2
    var current__178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__176)
    var t314 bool = current__178 < len__177
    var jp313 Option__string
    if t314 {
        var value__179 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__175, current__178)
        var t315 int32 = current__178 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__176, t315)
        var t316 Option__string = Option__string_Some{
            _0: value__179,
        }
        jp313 = t316
    } else {
        jp313 = Option__string_None{}
    }
    retv311 = jp313
    return retv311
}

func main() {
    main0()
}
