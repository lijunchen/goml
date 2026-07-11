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
    var retv47 int32
    retv47 = 7
    return retv47
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    var retv49 string
    retv49 = "seven"
    return retv49
}

func _goml_m_inherent_i_Counter_i_Counter_i_new(start__4 int32, end__5 int32) Counter {
    var retv51 Counter
    var t52 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__4)
    var t53 Counter = Counter{
        current: t52,
        end: end__5,
    }
    retv51 = t53
    return retv51
}

func _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_Counter_i_next(self__6 Counter) Option__int32 {
    var retv55 Option__int32
    var t56 *ref_int32_x = self__6.current
    var current__7 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t56)
    var t59 int32 = self__6.end
    var t60 bool = current__7 < t59
    var jp58 Option__int32
    if t60 {
        var t61 *ref_int32_x = self__6.current
        var t62 int32 = current__7 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t61, t62)
        var t63 Option__int32 = Option__int32_Some{
            _0: current__7,
        }
        jp58 = t63
    } else {
        jp58 = Option__int32_None{}
    }
    retv55 = jp58
    return retv55
}

func main0() struct{} {
    var t65 Token = Token{}
    var t66 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t65)
    println__T_int32(t66)
    var t67 Token = Token{}
    var t68 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t67)
    println__T_string(t68)
    var t69 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t69)
    println__T_int32(converted__8)
    var t70 Any = Any{}
    var t71 string = _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(t70)
    println__T_string(t71)
    var t72 Any = Any{}
    var t73 string = _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(t72)
    println__T_string(t73)
    var t74 Any = Any{}
    var t75 string = _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(t74)
    println__T_string(t75)
    var t76 Counter = _goml_m_inherent_i_Counter_i_Counter_i_new(0, 8)
    var t77 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 MapIterator__int32__int32__Counter = iterator_map__A_int32__B_int32__I_Counter(t76, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t77, p0)
    })
    var t78 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t78, p0)
    })
    var limited__13 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(filtered__12, 3)
    var for_iter29 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = limited__13
    Loop_loop88:
    for {
        if true {
            var for_next30 Option__int32 = _goml_m_trait__impl_i_Iterator_h64cea1f51f7fcd8e241a289588b4bc56__Counter_i_next(for_iter29)
            switch for_next30.(type) {
            case Option__int32_None:
                break Loop_loop88
            case Option__int32_Some:
                var x31 int32 = for_next30.(Option__int32_Some)._0
                var value__14 int32 = x31
                println__T_int32(value__14)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop88
        }
    }
    var t80 FnIterator__int32 = _goml_m_range(1, 5)
    var t81 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int32 = _goml_m_iterator__fold____A__int32____I__FnIterator_l_int32_r_____T__int32(t80, 0, func(p0 int32, p1 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t81, p0, p1)
    })
    println__T_int32(sum__17)
    var t82 FnIterator__int32 = _goml_m_range(1, 4)
    var t83 closure_env_main_3 = closure_env_main_3{}
    var t84 MapIterator__int32__string__FnIterator__int32 = _goml_m_iterator__map____A__int32____B__string____I__FnIterator_l_int32_r_(t82, func(p0 int32) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t83, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_iterator__collect____I_hc63511b064a501b8bab4d8cd45946ed4_r_____T__string(t84)
    var for_iter35 FnIterator__string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(texts__19)
    Loop_loop86:
    for {
        if true {
            var for_next36 Option__string = _goml_m_trait__impl_i_Iterator_i__l_string_r__x40_FnIterator____string_i_next(for_iter35)
            switch for_next36.(type) {
            case Option__string_None:
                break Loop_loop86
            case Option__string_Some:
                var x37 string = for_next36.(Option__string_Some)._0
                var text__20 string = x37
                println__T_string(text__20)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop86
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv91 *ref_int32_x
    var t92 *ref_int32_x = ref__Ref_5int32(value__137)
    retv91 = t92
    return retv91
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv94 int32
    var t95 int32 = ref_get__Ref_5int32(self__138)
    retv94 = t95
    return retv94
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t102)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    var retv105 int32
    var t106 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(value__2)
    retv105 = t106
    return retv105
}

func _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(self__3 Any) string {
    var retv108 string
    retv108 = "marked"
    return retv108
}

func _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(self__3 Any) string {
    var retv110 string
    retv110 = "marked"
    return retv110
}

func _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(self__3 Any) string {
    var retv112 string
    retv112 = "marked"
    return retv112
}

func iterator_map__A_int32__B_int32__I_Counter(iterator__79 Counter, map_fn__80 func(int32) int32) MapIterator__int32__int32__Counter {
    var retv114 MapIterator__int32__int32__Counter
    var t115 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__79,
        map_fn: map_fn__80,
    }
    retv114 = t115
    return retv114
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__81 MapIterator__int32__int32__Counter, predicate__82 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv117 FilterIterator__int32__MapIterator__int32__int32__Counter
    var t118 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__81,
        predicate: predicate__82,
    }
    retv117 = t118
    return retv117
}

func _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(iterator__83 FilterIterator__int32__MapIterator__int32__int32__Counter, count__84 int32) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv120 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var t125 bool = count__84 > 0
    var jp122 int32
    if t125 {
        jp122 = count__84
    } else {
        jp122 = 0
    }
    var remaining__85 int32 = jp122
    var t123 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(remaining__85)
    var t124 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__83,
        remaining: t123,
    }
    retv120 = t124
    return retv120
}

func _goml_m_trait__impl_i_Iterator_h64cea1f51f7fcd8e241a289588b4bc56__Counter_i_next(self__77 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv127 Option__int32
    var t128 *ref_int32_x = self__77.remaining
    var remaining__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t128)
    var t131 bool = remaining__78 > 0
    var jp130 Option__int32
    if t131 {
        var t132 *ref_int32_x = self__77.remaining
        var t133 int32 = remaining__78 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t132, t133)
        var t134 FilterIterator__int32__MapIterator__int32__int32__Counter = self__77.iterator
        var t135 Option__int32 = _goml_m_trait__impl_i_Iterator_ha0f912ca689df9b3b8ca5cd6c8cb5062__Counter_i_next(t134)
        jp130 = t135
    } else {
        jp130 = Option__int32_None{}
    }
    retv127 = jp130
    return retv127
}

func _goml_m_iterator__fold____A__int32____I__FnIterator_l_int32_r_____T__int32(iterator__86 FnIterator__int32, initial__87 int32, combine__88 func(int32, int32) int32) int32 {
    var retv137 int32
    var accumulator__89 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(initial__87)
    var running__90 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop140:
    for {
        var t141 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__90)
        if t141 {
            var mtmp7 Option__int32 = _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_FnIterator____int32_i_next(iterator__86)
            switch mtmp7.(type) {
            case Option__int32_None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__90, false)
            case Option__int32_Some:
                var x8 int32 = mtmp7.(Option__int32_Some)._0
                var value__91 int32 = x8
                var t144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(accumulator__89)
                var t145 int32 = combine__88(t144, value__91)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(accumulator__89, t145)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop140
        }
    }
    var t139 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(accumulator__89)
    retv137 = t139
    return retv137
}

func _goml_m_range(start__141 int32, end__142 int32) FnIterator__int32 {
    var retv148 FnIterator__int32
    var current__143 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__141)
    var t149 closure_env_range_4 = closure_env_range_4{
        current_0: current__143,
        end_1: end__142,
    }
    var t150 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__env__range__4_i_closure__env__range__4_i_apply(t149)
    })
    retv148 = t150
    return retv148
}

func _goml_m_iterator__collect____I_hc63511b064a501b8bab4d8cd45946ed4_r_____T__string(iterator__92 MapIterator__int32__string__FnIterator__int32) *_goml_vec_string {
    var retv152 *_goml_vec_string
    var values__93 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var running__94 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop154:
    for {
        var t155 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__94)
        if t155 {
            var mtmp11 Option__string = _goml_m_trait__impl_i_Iterator_h9354344b90c1d7aaaf7c07845bd6d756____int32_i_next(iterator__92)
            switch mtmp11.(type) {
            case Option__string_None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__94, false)
            case Option__string_Some:
                var x12 string = mtmp11.(Option__string_Some)._0
                var value__95 string = x12
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__93, value__95)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop154
        }
    }
    retv152 = values__93
    return retv152
}

func _goml_m_iterator__map____A__int32____B__string____I__FnIterator_l_int32_r_(iterator__79 FnIterator__int32, map_fn__80 func(int32) string) MapIterator__int32__string__FnIterator__int32 {
    var retv160 MapIterator__int32__string__FnIterator__int32
    var t161 MapIterator__int32__string__FnIterator__int32 = MapIterator__int32__string__FnIterator__int32{
        iterator: iterator__79,
        map_fn: map_fn__80,
    }
    retv160 = t161
    return retv160
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv163 string
    var t164 string = _goml_runtime_core_int32_to_string(self__2)
    retv163 = t164
    return retv163
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__111 *_goml_vec_string) FnIterator__string {
    var retv166 FnIterator__string
    var index__112 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__113 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__111)
    var t167 closure_env_inherent_Vec_Vec_T_iter_T_string_5 = closure_env_inherent_Vec_Vec_T_iter_T_string_5{
        index_0: index__112,
        len_1: len__113,
        self_2: self__111,
    }
    var t168 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(func() Option__string {
        return _goml_m_inherent_i_closure__en_h08b6bd77817c533c5e285632edcbf64d_ring__5_i_apply(t167)
    })
    retv166 = t168
    return retv166
}

func _goml_m_trait__impl_i_Iterator_i__l_string_r__x40_FnIterator____string_i_next(self__72 FnIterator__string) Option__string {
    var retv170 Option__string
    var t171 func() Option__string = self__72.next_fn
    var t172 Option__string = t171()
    retv170 = t172
    return retv170
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv174 string
    var t175 string = _goml_runtime_core_int32_to_string(self__13)
    retv174 = t175
    return retv174
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv177 string
    retv177 = self__9
    return retv177
}

func _goml_m_trait__impl_i_Iterator_ha0f912ca689df9b3b8ca5cd6c8cb5062__Counter_i_next(self__75 FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv179 Option__int32
    Loop_loop181:
    for {
        if true {
            var t182 MapIterator__int32__int32__Counter = self__75.iterator
            var mtmp2 Option__int32 = _goml_m_trait__impl_i_Iterator_h09d531bb1a077a878664377380ed2a19__Counter_i_next(t182)
            switch mtmp2.(type) {
            case Option__int32_None:
                retv179 = Option__int32_None{}
                return retv179
            case Option__int32_Some:
                var x3 int32 = mtmp2.(Option__int32_Some)._0
                var value__76 int32 = x3
                var t185 func(int32) bool = self__75.predicate
                var t186 bool = t185(value__76)
                if t186 {
                    var t187 Option__int32 = Option__int32_Some{
                        _0: value__76,
                    }
                    retv179 = t187
                    return retv179
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop181
        }
    }
    retv179 = Option__int32_None{}
    return retv179
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__137 bool) *ref_bool_x {
    var retv189 *ref_bool_x
    var t190 *ref_bool_x = ref__Ref_4bool(value__137)
    retv189 = t190
    return retv189
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__138 *ref_bool_x) bool {
    var retv192 bool
    var t193 bool = ref_get__Ref_4bool(self__138)
    retv192 = t193
    return retv192
}

func _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_FnIterator____int32_i_next(self__72 FnIterator__int32) Option__int32 {
    var retv195 Option__int32
    var t196 func() Option__int32 = self__72.next_fn
    var t197 Option__int32 = t196()
    retv195 = t197
    return retv195
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__139 *ref_bool_x, value__140 bool) struct{} {
    ref_set__Ref_4bool(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__71 func() Option__int32) FnIterator__int32 {
    var retv201 FnIterator__int32
    var t202 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__71,
    }
    retv201 = t202
    return retv201
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv204 *_goml_vec_string
    var t205 *_goml_vec_string = vec_new__Vec_6string()
    retv204 = t205
    return retv204
}

func _goml_m_trait__impl_i_Iterator_h9354344b90c1d7aaaf7c07845bd6d756____int32_i_next(self__73 MapIterator__int32__string__FnIterator__int32) Option__string {
    var retv207 Option__string
    var t208 FnIterator__int32 = self__73.iterator
    var mtmp0 Option__int32 = _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_FnIterator____int32_i_next(t208)
    var jp210 Option__string
    switch mtmp0.(type) {
    case Option__int32_None:
        jp210 = Option__string_None{}
    case Option__int32_Some:
        var x1 int32 = mtmp0.(Option__int32_Some)._0
        var value__74 int32 = x1
        var t211 func(int32) string = self__73.map_fn
        var t212 string = t211(value__74)
        var t213 Option__string = Option__string_Some{
            _0: t212,
        }
        jp210 = t213
    default:
        panic("non-exhaustive match")
    }
    retv207 = jp210
    return retv207
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__96 *_goml_vec_string, elem__97 string) struct{} {
    vec_push__Vec_6string(self__96, elem__97)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__107 *_goml_vec_string) int32 {
    var retv217 int32
    var t218 int32 = vec_len__Vec_6string(self__107)
    retv217 = t218
    return retv217
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__102 *_goml_vec_string, index__103 int32) string {
    var retv220 string
    var t221 string = vec_get__Vec_6string(self__102, index__103)
    retv220 = t221
    return retv220
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__71 func() Option__string) FnIterator__string {
    var retv223 FnIterator__string
    var t224 FnIterator__string = FnIterator__string{
        next_fn: next_fn__71,
    }
    retv223 = t224
    return retv223
}

func _goml_m_trait__impl_i_Iterator_h09d531bb1a077a878664377380ed2a19__Counter_i_next(self__73 MapIterator__int32__int32__Counter) Option__int32 {
    var retv226 Option__int32
    var t227 Counter = self__73.iterator
    var mtmp0 Option__int32 = _goml_m_trait__impl_i_Iterator_i__l_int32_r__x40_Counter_i_next(t227)
    var jp229 Option__int32
    switch mtmp0.(type) {
    case Option__int32_None:
        jp229 = Option__int32_None{}
    case Option__int32_Some:
        var x1 int32 = mtmp0.(Option__int32_Some)._0
        var value__74 int32 = x1
        var t230 func(int32) int32 = self__73.map_fn
        var t231 int32 = t230(value__74)
        var t232 Option__int32 = Option__int32_Some{
            _0: t231,
        }
        jp229 = t232
    default:
        panic("non-exhaustive match")
    }
    retv226 = jp229
    return retv226
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env40 closure_env_main_0, value__9 int32) int32 {
    var retv247 int32
    var t248 int32 = value__9 * 2
    retv247 = t248
    return retv247
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env41 closure_env_main_1, value__11 int32) bool {
    var retv250 bool
    var t251 bool = value__11 > 4
    retv250 = t251
    return retv250
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env42 closure_env_main_2, total__15 int32, value__16 int32) int32 {
    var retv253 int32
    var t254 int32 = total__15 + value__16
    retv253 = t254
    return retv253
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env43 closure_env_main_3, value__18 int32) string {
    var retv256 string
    var t257 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__18)
    var t258 string = "v" + t257
    retv256 = t258
    return retv256
}

func _goml_m_inherent_i_closure__env__range__4_i_closure__env__range__4_i_apply(env44 closure_env_range_4) Option__int32 {
    var retv260 Option__int32
    var current__143 *ref_int32_x = env44.current_0
    var end__142 int32 = env44.end_1
    var value__144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__143)
    var t263 bool = value__144 < end__142
    var jp262 Option__int32
    if t263 {
        var t264 int32 = value__144 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__143, t264)
        var t265 Option__int32 = Option__int32_Some{
            _0: value__144,
        }
        jp262 = t265
    } else {
        jp262 = Option__int32_None{}
    }
    retv260 = jp262
    return retv260
}

func _goml_m_inherent_i_closure__en_h08b6bd77817c533c5e285632edcbf64d_ring__5_i_apply(env45 closure_env_inherent_Vec_Vec_T_iter_T_string_5) Option__string {
    var retv267 Option__string
    var index__112 *ref_int32_x = env45.index_0
    var len__113 int32 = env45.len_1
    var self__111 *_goml_vec_string = env45.self_2
    var current__114 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__112)
    var t270 bool = current__114 < len__113
    var jp269 Option__string
    if t270 {
        var value__115 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__111, current__114)
        var t271 int32 = current__114 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__112, t271)
        var t272 Option__string = Option__string_Some{
            _0: value__115,
        }
        jp269 = t272
    } else {
        jp269 = Option__string_None{}
    }
    retv267 = jp269
    return retv267
}

func main() {
    main0()
}
