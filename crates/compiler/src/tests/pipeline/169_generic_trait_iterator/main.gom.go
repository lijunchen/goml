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
    var retv83 int32
    retv83 = 7
    return retv83
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    var retv85 string
    retv85 = "seven"
    return retv85
}

func _goml_m_inherent_i_Counter_i_Counter_i_new(start__4 int32, end__5 int32) Counter {
    var retv87 Counter
    var t88 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__4)
    var t89 Counter = Counter{
        current: t88,
        end: end__5,
    }
    retv87 = t89
    return retv87
}

func _goml_m_trait__impl_i_Iterator_i_Counter_i_next(self__6 Counter) Option__int32 {
    var retv91 Option__int32
    var t92 *ref_int32_x = self__6.current
    var current__7 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t92)
    var t95 int32 = self__6.end
    var t96 bool = current__7 < t95
    var jp94 Option__int32
    if t96 {
        var t97 *ref_int32_x = self__6.current
        var t98 int32 = current__7 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t97, t98)
        var t99 Option__int32 = Option__int32_Some{
            _0: current__7,
        }
        jp94 = t99
    } else {
        jp94 = Option__int32_None{}
    }
    retv91 = jp94
    return retv91
}

func main0() struct{} {
    var t101 Token = Token{}
    var t102 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t101)
    println__T_int32(t102)
    var t103 Token = Token{}
    var t104 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t103)
    println__T_string(t104)
    var t105 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t105)
    println__T_int32(converted__8)
    var t106 Any = Any{}
    var t107 string = _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(t106)
    println__T_string(t107)
    var t108 Any = Any{}
    var t109 string = _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(t108)
    println__T_string(t109)
    var t110 Any = Any{}
    var t111 string = _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(t110)
    println__T_string(t111)
    var t112 Counter = _goml_m_inherent_i_Counter_i_Counter_i_new(0, 8)
    var t113 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 MapIterator__int32__int32__Counter = iterator_map__A_int32__B_int32__I_Counter(t112, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t113, p0)
    })
    var t114 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t114, p0)
    })
    var limited__13 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(filtered__12, 3)
    var for_iter65 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(limited__13)
    Loop_loop124:
    for {
        if true {
            var for_next66 Option__int32 = _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(for_iter65)
            switch for_next66.(type) {
            case Option__int32_None:
                break Loop_loop124
            case Option__int32_Some:
                var x67 int32 = for_next66.(Option__int32_Some)._0
                var value__14 int32 = x67
                println__T_int32(value__14)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop124
        }
    }
    var t116 FnIterator__int32 = _goml_m_range(1, 5)
    var t117 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int32 = _goml_m_iterator__fold____A__int32____I__FnIterator_l_int32_r_____T__int32(t116, 0, func(p0 int32, p1 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t117, p0, p1)
    })
    println__T_int32(sum__17)
    var t118 FnIterator__int32 = _goml_m_range(1, 4)
    var t119 closure_env_main_3 = closure_env_main_3{}
    var t120 MapIterator__int32__string__FnIterator__int32 = _goml_m_iterator__map____A__int32____B__string____I__FnIterator_l_int32_r_(t118, func(p0 int32) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t119, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_iterator__collect____I_hc63511b064a501b8bab4d8cd45946ed4_r_____T__string(t120)
    var for_iter71 FnIterator__string = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(texts__19)
    Loop_loop122:
    for {
        if true {
            var for_next72 Option__string = _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(for_iter71)
            switch for_next72.(type) {
            case Option__string_None:
                break Loop_loop122
            case Option__string_Some:
                var x73 string = for_next72.(Option__string_Some)._0
                var text__20 string = x73
                println__T_string(text__20)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop122
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv127 *ref_int32_x
    var t128 *ref_int32_x = ref__Ref_5int32(value__200)
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv130 int32
    var t131 int32 = ref_get__Ref_5int32(self__201)
    retv130 = t131
    return retv130
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t135 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t135)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t138 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t138)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    var retv141 int32
    var t142 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(value__2)
    retv141 = t142
    return retv141
}

func _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(self__3 Any) string {
    var retv144 string
    retv144 = "marked"
    return retv144
}

func _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(self__3 Any) string {
    var retv146 string
    retv146 = "marked"
    return retv146
}

func _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(self__3 Any) string {
    var retv148 string
    retv148 = "marked"
    return retv148
}

func iterator_map__A_int32__B_int32__I_Counter(iterator__102 Counter, map_fn__103 func(int32) int32) MapIterator__int32__int32__Counter {
    var retv150 MapIterator__int32__int32__Counter
    var t151 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__102,
        map_fn: map_fn__103,
    }
    retv150 = t151
    return retv150
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__104 MapIterator__int32__int32__Counter, predicate__105 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv153 FilterIterator__int32__MapIterator__int32__int32__Counter
    var t154 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__104,
        predicate: predicate__105,
    }
    retv153 = t154
    return retv153
}

func _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(iterator__106 FilterIterator__int32__MapIterator__int32__int32__Counter, count__107 int32) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv156 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var t161 bool = count__107 > 0
    var jp158 int32
    if t161 {
        jp158 = count__107
    } else {
        jp158 = 0
    }
    var remaining__108 int32 = jp158
    var t159 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(remaining__108)
    var t160 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__106,
        remaining: t159,
    }
    retv156 = t160
    return retv156
}

func _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(self__101 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv163 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    retv163 = self__101
    return retv163
}

func _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(self__99 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv165 Option__int32
    var t166 *ref_int32_x = self__99.remaining
    var remaining__100 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t166)
    var t169 bool = remaining__100 > 0
    var jp168 Option__int32
    if t169 {
        var t170 *ref_int32_x = self__99.remaining
        var t171 int32 = remaining__100 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t170, t171)
        var t172 FilterIterator__int32__MapIterator__int32__int32__Counter = self__99.iterator
        var t173 Option__int32 = _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(t172)
        jp168 = t173
    } else {
        jp168 = Option__int32_None{}
    }
    retv165 = jp168
    return retv165
}

func _goml_m_iterator__fold____A__int32____I__FnIterator_l_int32_r_____T__int32(iterator__109 FnIterator__int32, initial__110 int32, combine__111 func(int32, int32) int32) int32 {
    var retv175 int32
    var accumulator__112 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(initial__110)
    var running__113 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop178:
    for {
        var t179 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__113)
        if t179 {
            var mtmp23 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(iterator__109)
            switch mtmp23.(type) {
            case Option__int32_None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__113, false)
            case Option__int32_Some:
                var x24 int32 = mtmp23.(Option__int32_Some)._0
                var value__114 int32 = x24
                var t182 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(accumulator__112)
                var t183 int32 = combine__111(t182, value__114)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(accumulator__112, t183)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop178
        }
    }
    var t177 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(accumulator__112)
    retv175 = t177
    return retv175
}

func _goml_m_range(start__204 int32, end__205 int32) FnIterator__int32 {
    var retv186 FnIterator__int32
    var current__206 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__204)
    var t187 closure_env_range_4 = closure_env_range_4{
        current_0: current__206,
        end_1: end__205,
    }
    var t188 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__env__range__4_i_closure__env__range__4_i_apply(t187)
    })
    retv186 = t188
    return retv186
}

func _goml_m_iterator__collect____I_hc63511b064a501b8bab4d8cd45946ed4_r_____T__string(iterator__115 MapIterator__int32__string__FnIterator__int32) *_goml_vec_string {
    var retv190 *_goml_vec_string
    var values__116 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var running__117 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop192:
    for {
        var t193 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__117)
        if t193 {
            var mtmp27 Option__string = _goml_m_trait__impl_i_Iterator_had40bfe6daf831512a74e0ce237eceb9____int32_i_next(iterator__115)
            switch mtmp27.(type) {
            case Option__string_None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__117, false)
            case Option__string_Some:
                var x28 string = mtmp27.(Option__string_Some)._0
                var value__118 string = x28
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__116, value__118)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop192
        }
    }
    retv190 = values__116
    return retv190
}

func _goml_m_iterator__map____A__int32____B__string____I__FnIterator_l_int32_r_(iterator__102 FnIterator__int32, map_fn__103 func(int32) string) MapIterator__int32__string__FnIterator__int32 {
    var retv198 MapIterator__int32__string__FnIterator__int32
    var t199 MapIterator__int32__string__FnIterator__int32 = MapIterator__int32__string__FnIterator__int32{
        iterator: iterator__102,
        map_fn: map_fn__103,
    }
    retv198 = t199
    return retv198
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv201 string
    var t202 string = _goml_runtime_core_int32_to_string(self__2)
    retv201 = t202
    return retv201
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(self__176 *_goml_vec_string) FnIterator__string {
    var retv204 FnIterator__string
    var t205 FnIterator__string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__176)
    retv204 = t205
    return retv204
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(self__94 FnIterator__string) Option__string {
    var retv207 Option__string
    var t208 func() Option__string = self__94.next_fn
    var t209 Option__string = t208()
    retv207 = t209
    return retv207
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv211 string
    var t212 string = _goml_runtime_core_int32_to_string(self__38)
    retv211 = t212
    return retv211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv214 string
    retv214 = self__34
    return retv214
}

func _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(self__97 FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv216 Option__int32
    Loop_loop218:
    for {
        if true {
            var t219 MapIterator__int32__int32__Counter = self__97.iterator
            var mtmp18 Option__int32 = _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(t219)
            switch mtmp18.(type) {
            case Option__int32_None:
                retv216 = Option__int32_None{}
                return retv216
            case Option__int32_Some:
                var x19 int32 = mtmp18.(Option__int32_Some)._0
                var value__98 int32 = x19
                var t222 func(int32) bool = self__97.predicate
                var t223 bool = t222(value__98)
                if t223 {
                    var t224 Option__int32 = Option__int32_Some{
                        _0: value__98,
                    }
                    retv216 = t224
                    return retv216
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop218
        }
    }
    retv216 = Option__int32_None{}
    return retv216
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__200 bool) *ref_bool_x {
    var retv226 *ref_bool_x
    var t227 *ref_bool_x = ref__Ref_4bool(value__200)
    retv226 = t227
    return retv226
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__201 *ref_bool_x) bool {
    var retv229 bool
    var t230 bool = ref_get__Ref_4bool(self__201)
    retv229 = t230
    return retv229
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__94 FnIterator__int32) Option__int32 {
    var retv232 Option__int32
    var t233 func() Option__int32 = self__94.next_fn
    var t234 Option__int32 = t233()
    retv232 = t234
    return retv232
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__202 *ref_bool_x, value__203 bool) struct{} {
    ref_set__Ref_4bool(self__202, value__203)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__93 func() Option__int32) FnIterator__int32 {
    var retv238 FnIterator__int32
    var t239 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__93,
    }
    retv238 = t239
    return retv238
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv241 *_goml_vec_string
    var t242 *_goml_vec_string = vec_new__Vec_6string()
    retv241 = t242
    return retv241
}

func _goml_m_trait__impl_i_Iterator_had40bfe6daf831512a74e0ce237eceb9____int32_i_next(self__95 MapIterator__int32__string__FnIterator__int32) Option__string {
    var retv244 Option__string
    var t245 FnIterator__int32 = self__95.iterator
    var mtmp16 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(t245)
    var jp247 Option__string
    switch mtmp16.(type) {
    case Option__int32_None:
        jp247 = Option__string_None{}
    case Option__int32_Some:
        var x17 int32 = mtmp16.(Option__int32_Some)._0
        var value__96 int32 = x17
        var t248 func(int32) string = self__95.map_fn
        var t249 string = t248(value__96)
        var t250 Option__string = Option__string_Some{
            _0: t249,
        }
        jp247 = t250
    default:
        panic("non-exhaustive match")
    }
    retv244 = jp247
    return retv244
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__120 *_goml_vec_string, elem__121 string) struct{} {
    vec_push__Vec_6string(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__171 *_goml_vec_string) FnIterator__string {
    var retv254 FnIterator__string
    var index__172 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__173 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__171)
    var t255 closure_env_inherent_Vec_Vec_T_iter_T_string_5 = closure_env_inherent_Vec_Vec_T_iter_T_string_5{
        index_0: index__172,
        len_1: len__173,
        self_2: self__171,
    }
    var t256 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(func() Option__string {
        return _goml_m_inherent_i_closure__en_h08b6bd77817c533c5e285632edcbf64d_ring__5_i_apply(t255)
    })
    retv254 = t256
    return retv254
}

func _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(self__95 MapIterator__int32__int32__Counter) Option__int32 {
    var retv258 Option__int32
    var t259 Counter = self__95.iterator
    var mtmp16 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Counter_i_next(t259)
    var jp261 Option__int32
    switch mtmp16.(type) {
    case Option__int32_None:
        jp261 = Option__int32_None{}
    case Option__int32_Some:
        var x17 int32 = mtmp16.(Option__int32_Some)._0
        var value__96 int32 = x17
        var t262 func(int32) int32 = self__95.map_fn
        var t263 int32 = t262(value__96)
        var t264 Option__int32 = Option__int32_Some{
            _0: t263,
        }
        jp261 = t264
    default:
        panic("non-exhaustive match")
    }
    retv258 = jp261
    return retv258
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__131 *_goml_vec_string) int32 {
    var retv266 int32
    var t267 int32 = vec_len__Vec_6string(self__131)
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__126 *_goml_vec_string, index__127 int32) string {
    var retv269 string
    var t270 string = vec_get__Vec_6string(self__126, index__127)
    retv269 = t270
    return retv269
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__93 func() Option__string) FnIterator__string {
    var retv272 FnIterator__string
    var t273 FnIterator__string = FnIterator__string{
        next_fn: next_fn__93,
    }
    retv272 = t273
    return retv272
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env76 closure_env_main_0, value__9 int32) int32 {
    var retv288 int32
    var t289 int32 = value__9 * 2
    retv288 = t289
    return retv288
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env77 closure_env_main_1, value__11 int32) bool {
    var retv291 bool
    var t292 bool = value__11 > 4
    retv291 = t292
    return retv291
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env78 closure_env_main_2, total__15 int32, value__16 int32) int32 {
    var retv294 int32
    var t295 int32 = total__15 + value__16
    retv294 = t295
    return retv294
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env79 closure_env_main_3, value__18 int32) string {
    var retv297 string
    var t298 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__18)
    var t299 string = "v" + t298
    retv297 = t299
    return retv297
}

func _goml_m_inherent_i_closure__env__range__4_i_closure__env__range__4_i_apply(env80 closure_env_range_4) Option__int32 {
    var retv301 Option__int32
    var current__206 *ref_int32_x = env80.current_0
    var end__205 int32 = env80.end_1
    var value__207 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__206)
    var t304 bool = value__207 < end__205
    var jp303 Option__int32
    if t304 {
        var t305 int32 = value__207 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__206, t305)
        var t306 Option__int32 = Option__int32_Some{
            _0: value__207,
        }
        jp303 = t306
    } else {
        jp303 = Option__int32_None{}
    }
    retv301 = jp303
    return retv301
}

func _goml_m_inherent_i_closure__en_h08b6bd77817c533c5e285632edcbf64d_ring__5_i_apply(env81 closure_env_inherent_Vec_Vec_T_iter_T_string_5) Option__string {
    var retv308 Option__string
    var index__172 *ref_int32_x = env81.index_0
    var len__173 int32 = env81.len_1
    var self__171 *_goml_vec_string = env81.self_2
    var current__174 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__172)
    var t311 bool = current__174 < len__173
    var jp310 Option__string
    if t311 {
        var value__175 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__171, current__174)
        var t312 int32 = current__174 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__172, t312)
        var t313 Option__string = Option__string_Some{
            _0: value__175,
        }
        jp310 = t313
    } else {
        jp310 = Option__string_None{}
    }
    retv308 = jp310
    return retv308
}

func main() {
    main0()
}
