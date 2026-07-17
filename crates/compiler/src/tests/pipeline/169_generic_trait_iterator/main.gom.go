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

func _goml_m_trait__impl_i_Iterator_i_Counter_i_next(self__6 Counter) Option__int32 {
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
    var for_iter29 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(limited__13)
    Loop_loop88:
    for {
        if true {
            var for_next30 Option__int32 = _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(for_iter29)
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
    var for_iter35 FnIterator__string = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(texts__19)
    Loop_loop86:
    for {
        if true {
            var for_next36 Option__string = _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(for_iter35)
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

func iterator_map__A_int32__B_int32__I_Counter(iterator__77 Counter, map_fn__78 func(int32) int32) MapIterator__int32__int32__Counter {
    var retv114 MapIterator__int32__int32__Counter
    var t115 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__77,
        map_fn: map_fn__78,
    }
    retv114 = t115
    return retv114
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__79 MapIterator__int32__int32__Counter, predicate__80 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv117 FilterIterator__int32__MapIterator__int32__int32__Counter
    var t118 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__79,
        predicate: predicate__80,
    }
    retv117 = t118
    return retv117
}

func _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(iterator__81 FilterIterator__int32__MapIterator__int32__int32__Counter, count__82 int32) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv120 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var t125 bool = count__82 > 0
    var jp122 int32
    if t125 {
        jp122 = count__82
    } else {
        jp122 = 0
    }
    var remaining__83 int32 = jp122
    var t123 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(remaining__83)
    var t124 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__81,
        remaining: t123,
    }
    retv120 = t124
    return retv120
}

func _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(self__76 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv127 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    retv127 = self__76
    return retv127
}

func _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(self__74 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv129 Option__int32
    var t130 *ref_int32_x = self__74.remaining
    var remaining__75 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t130)
    var t133 bool = remaining__75 > 0
    var jp132 Option__int32
    if t133 {
        var t134 *ref_int32_x = self__74.remaining
        var t135 int32 = remaining__75 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t134, t135)
        var t136 FilterIterator__int32__MapIterator__int32__int32__Counter = self__74.iterator
        var t137 Option__int32 = _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(t136)
        jp132 = t137
    } else {
        jp132 = Option__int32_None{}
    }
    retv129 = jp132
    return retv129
}

func _goml_m_iterator__fold____A__int32____I__FnIterator_l_int32_r_____T__int32(iterator__84 FnIterator__int32, initial__85 int32, combine__86 func(int32, int32) int32) int32 {
    var retv139 int32
    var accumulator__87 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(initial__85)
    var running__88 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop142:
    for {
        var t143 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__88)
        if t143 {
            var mtmp7 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(iterator__84)
            switch mtmp7.(type) {
            case Option__int32_None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__88, false)
            case Option__int32_Some:
                var x8 int32 = mtmp7.(Option__int32_Some)._0
                var value__89 int32 = x8
                var t146 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(accumulator__87)
                var t147 int32 = combine__86(t146, value__89)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(accumulator__87, t147)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop142
        }
    }
    var t141 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(accumulator__87)
    retv139 = t141
    return retv139
}

func _goml_m_range(start__141 int32, end__142 int32) FnIterator__int32 {
    var retv150 FnIterator__int32
    var current__143 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__141)
    var t151 closure_env_range_4 = closure_env_range_4{
        current_0: current__143,
        end_1: end__142,
    }
    var t152 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__env__range__4_i_closure__env__range__4_i_apply(t151)
    })
    retv150 = t152
    return retv150
}

func _goml_m_iterator__collect____I_hc63511b064a501b8bab4d8cd45946ed4_r_____T__string(iterator__90 MapIterator__int32__string__FnIterator__int32) *_goml_vec_string {
    var retv154 *_goml_vec_string
    var values__91 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var running__92 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop156:
    for {
        var t157 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__92)
        if t157 {
            var mtmp11 Option__string = _goml_m_trait__impl_i_Iterator_had40bfe6daf831512a74e0ce237eceb9____int32_i_next(iterator__90)
            switch mtmp11.(type) {
            case Option__string_None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__92, false)
            case Option__string_Some:
                var x12 string = mtmp11.(Option__string_Some)._0
                var value__93 string = x12
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__91, value__93)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop156
        }
    }
    retv154 = values__91
    return retv154
}

func _goml_m_iterator__map____A__int32____B__string____I__FnIterator_l_int32_r_(iterator__77 FnIterator__int32, map_fn__78 func(int32) string) MapIterator__int32__string__FnIterator__int32 {
    var retv162 MapIterator__int32__string__FnIterator__int32
    var t163 MapIterator__int32__string__FnIterator__int32 = MapIterator__int32__string__FnIterator__int32{
        iterator: iterator__77,
        map_fn: map_fn__78,
    }
    retv162 = t163
    return retv162
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv165 string
    var t166 string = _goml_runtime_core_int32_to_string(self__2)
    retv165 = t166
    return retv165
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(self__114 *_goml_vec_string) FnIterator__string {
    var retv168 FnIterator__string
    var t169 FnIterator__string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__114)
    retv168 = t169
    return retv168
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(self__69 FnIterator__string) Option__string {
    var retv171 Option__string
    var t172 func() Option__string = self__69.next_fn
    var t173 Option__string = t172()
    retv171 = t173
    return retv171
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv175 string
    var t176 string = _goml_runtime_core_int32_to_string(self__13)
    retv175 = t176
    return retv175
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv178 string
    retv178 = self__9
    return retv178
}

func _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(self__72 FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv180 Option__int32
    Loop_loop182:
    for {
        if true {
            var t183 MapIterator__int32__int32__Counter = self__72.iterator
            var mtmp2 Option__int32 = _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(t183)
            switch mtmp2.(type) {
            case Option__int32_None:
                retv180 = Option__int32_None{}
                return retv180
            case Option__int32_Some:
                var x3 int32 = mtmp2.(Option__int32_Some)._0
                var value__73 int32 = x3
                var t186 func(int32) bool = self__72.predicate
                var t187 bool = t186(value__73)
                if t187 {
                    var t188 Option__int32 = Option__int32_Some{
                        _0: value__73,
                    }
                    retv180 = t188
                    return retv180
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop182
        }
    }
    retv180 = Option__int32_None{}
    return retv180
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__137 bool) *ref_bool_x {
    var retv190 *ref_bool_x
    var t191 *ref_bool_x = ref__Ref_4bool(value__137)
    retv190 = t191
    return retv190
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__138 *ref_bool_x) bool {
    var retv193 bool
    var t194 bool = ref_get__Ref_4bool(self__138)
    retv193 = t194
    return retv193
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__69 FnIterator__int32) Option__int32 {
    var retv196 Option__int32
    var t197 func() Option__int32 = self__69.next_fn
    var t198 Option__int32 = t197()
    retv196 = t198
    return retv196
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__139 *ref_bool_x, value__140 bool) struct{} {
    ref_set__Ref_4bool(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__68 func() Option__int32) FnIterator__int32 {
    var retv202 FnIterator__int32
    var t203 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__68,
    }
    retv202 = t203
    return retv202
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv205 *_goml_vec_string
    var t206 *_goml_vec_string = vec_new__Vec_6string()
    retv205 = t206
    return retv205
}

func _goml_m_trait__impl_i_Iterator_had40bfe6daf831512a74e0ce237eceb9____int32_i_next(self__70 MapIterator__int32__string__FnIterator__int32) Option__string {
    var retv208 Option__string
    var t209 FnIterator__int32 = self__70.iterator
    var mtmp0 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(t209)
    var jp211 Option__string
    switch mtmp0.(type) {
    case Option__int32_None:
        jp211 = Option__string_None{}
    case Option__int32_Some:
        var x1 int32 = mtmp0.(Option__int32_Some)._0
        var value__71 int32 = x1
        var t212 func(int32) string = self__70.map_fn
        var t213 string = t212(value__71)
        var t214 Option__string = Option__string_Some{
            _0: t213,
        }
        jp211 = t214
    default:
        panic("non-exhaustive match")
    }
    retv208 = jp211
    return retv208
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__94 *_goml_vec_string, elem__95 string) struct{} {
    vec_push__Vec_6string(self__94, elem__95)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__109 *_goml_vec_string) FnIterator__string {
    var retv218 FnIterator__string
    var index__110 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__111 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__109)
    var t219 closure_env_inherent_Vec_Vec_T_iter_T_string_5 = closure_env_inherent_Vec_Vec_T_iter_T_string_5{
        index_0: index__110,
        len_1: len__111,
        self_2: self__109,
    }
    var t220 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(func() Option__string {
        return _goml_m_inherent_i_closure__en_h08b6bd77817c533c5e285632edcbf64d_ring__5_i_apply(t219)
    })
    retv218 = t220
    return retv218
}

func _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(self__70 MapIterator__int32__int32__Counter) Option__int32 {
    var retv222 Option__int32
    var t223 Counter = self__70.iterator
    var mtmp0 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Counter_i_next(t223)
    var jp225 Option__int32
    switch mtmp0.(type) {
    case Option__int32_None:
        jp225 = Option__int32_None{}
    case Option__int32_Some:
        var x1 int32 = mtmp0.(Option__int32_Some)._0
        var value__71 int32 = x1
        var t226 func(int32) int32 = self__70.map_fn
        var t227 int32 = t226(value__71)
        var t228 Option__int32 = Option__int32_Some{
            _0: t227,
        }
        jp225 = t228
    default:
        panic("non-exhaustive match")
    }
    retv222 = jp225
    return retv222
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__105 *_goml_vec_string) int32 {
    var retv230 int32
    var t231 int32 = vec_len__Vec_6string(self__105)
    retv230 = t231
    return retv230
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__100 *_goml_vec_string, index__101 int32) string {
    var retv233 string
    var t234 string = vec_get__Vec_6string(self__100, index__101)
    retv233 = t234
    return retv233
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__68 func() Option__string) FnIterator__string {
    var retv236 FnIterator__string
    var t237 FnIterator__string = FnIterator__string{
        next_fn: next_fn__68,
    }
    retv236 = t237
    return retv236
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env40 closure_env_main_0, value__9 int32) int32 {
    var retv252 int32
    var t253 int32 = value__9 * 2
    retv252 = t253
    return retv252
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env41 closure_env_main_1, value__11 int32) bool {
    var retv255 bool
    var t256 bool = value__11 > 4
    retv255 = t256
    return retv255
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env42 closure_env_main_2, total__15 int32, value__16 int32) int32 {
    var retv258 int32
    var t259 int32 = total__15 + value__16
    retv258 = t259
    return retv258
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env43 closure_env_main_3, value__18 int32) string {
    var retv261 string
    var t262 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__18)
    var t263 string = "v" + t262
    retv261 = t263
    return retv261
}

func _goml_m_inherent_i_closure__env__range__4_i_closure__env__range__4_i_apply(env44 closure_env_range_4) Option__int32 {
    var retv265 Option__int32
    var current__143 *ref_int32_x = env44.current_0
    var end__142 int32 = env44.end_1
    var value__144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__143)
    var t268 bool = value__144 < end__142
    var jp267 Option__int32
    if t268 {
        var t269 int32 = value__144 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__143, t269)
        var t270 Option__int32 = Option__int32_Some{
            _0: value__144,
        }
        jp267 = t270
    } else {
        jp267 = Option__int32_None{}
    }
    retv265 = jp267
    return retv265
}

func _goml_m_inherent_i_closure__en_h08b6bd77817c533c5e285632edcbf64d_ring__5_i_apply(env45 closure_env_inherent_Vec_Vec_T_iter_T_string_5) Option__string {
    var retv272 Option__string
    var index__110 *ref_int32_x = env45.index_0
    var len__111 int32 = env45.len_1
    var self__109 *_goml_vec_string = env45.self_2
    var current__112 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__110)
    var t275 bool = current__112 < len__111
    var jp274 Option__string
    if t275 {
        var value__113 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__109, current__112)
        var t276 int32 = current__112 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__110, t276)
        var t277 Option__string = Option__string_Some{
            _0: value__113,
        }
        jp274 = t277
    } else {
        jp274 = Option__string_None{}
    }
    retv272 = jp274
    return retv272
}

func main() {
    main0()
}
