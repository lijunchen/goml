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
    remaining *ref_int_x
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type MapIterator__int__string__FnIterator__int struct {
    iterator FnIterator__int
    map_fn func(int) string
}

type FnIterator__string struct {
    next_fn func() Option__string
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {}

type closure_env_goml_builtin_range_4 struct {
    current_0 *ref_int_x
    end_1 int
}

type closure_env_inherent_Vec_Vec_T_iter_T_string_5 struct {
    index_0 *ref_int_x
    len_1 int
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
    var retv89 int32
    retv89 = 7
    return retv89
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    var retv91 string
    retv91 = "seven"
    return retv91
}

func _goml_m_inherent_i_Counter_i_Counter_i_new(start__4 int32, end__5 int32) Counter {
    var retv93 Counter
    var t94 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__4)
    var t95 Counter = Counter{
        current: t94,
        end: end__5,
    }
    retv93 = t95
    return retv93
}

func _goml_m_trait__impl_i_Iterator_i_Counter_i_next(self__6 Counter) Option__int32 {
    var retv97 Option__int32
    var t98 *ref_int32_x = self__6.current
    var current__7 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t98)
    var t101 int32 = self__6.end
    var t102 bool = current__7 < t101
    var jp100 Option__int32
    if t102 {
        var t103 *ref_int32_x = self__6.current
        var t104 int32 = current__7 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t103, t104)
        var t105 Option__int32 = Option__int32_Some{
            _0: current__7,
        }
        jp100 = t105
    } else {
        jp100 = Option__int32_None{}
    }
    retv97 = jp100
    return retv97
}

func main0() struct{} {
    var t107 Token = Token{}
    var t108 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t107)
    println__T_int32(t108)
    var t109 Token = Token{}
    var t110 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t109)
    println__T_string(t110)
    var t111 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t111)
    println__T_int32(converted__8)
    var t112 Any = Any{}
    var t113 string = _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(t112)
    println__T_string(t113)
    var t114 Any = Any{}
    var t115 string = _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(t114)
    println__T_string(t115)
    var t116 Any = Any{}
    var t117 string = _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(t116)
    println__T_string(t117)
    var t118 Counter = _goml_m_inherent_i_Counter_i_Counter_i_new(0, 8)
    var t119 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 MapIterator__int32__int32__Counter = iterator_map__A_int32__B_int32__I_Counter(t118, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t119, p0)
    })
    var t120 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t120, p0)
    })
    var limited__13 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(filtered__12, 3)
    var for_iter71 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(limited__13)
    Loop_loop130:
    for {
        if true {
            var for_next72 Option__int32 = _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(for_iter71)
            switch for_next72.(type) {
            case Option__int32_None:
                break Loop_loop130
            case Option__int32_Some:
                var x73 int32 = for_next72.(Option__int32_Some)._0
                var value__14 int32 = x73
                println__T_int32(value__14)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop130
        }
    }
    var t122 FnIterator__int = _goml_m_range(1, 5)
    var t123 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t122, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t123, p0, p1)
    })
    println__T_int(sum__17)
    var t124 FnIterator__int = _goml_m_range(1, 4)
    var t125 closure_env_main_3 = closure_env_main_3{}
    var t126 MapIterator__int__string__FnIterator__int = _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(t124, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t125, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(t126)
    var for_iter77 FnIterator__string = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(texts__19)
    Loop_loop128:
    for {
        if true {
            var for_next78 Option__string = _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(for_iter77)
            switch for_next78.(type) {
            case Option__string_None:
                break Loop_loop128
            case Option__string_Some:
                var x79 string = for_next78.(Option__string_Some)._0
                var text__20 string = x79
                println__T_string(text__20)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop128
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv133 *ref_int32_x
    var t134 *ref_int32_x = ref__Ref_5int32(value__209)
    retv133 = t134
    return retv133
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv136 int32
    var t137 int32 = ref_get__Ref_5int32(self__210)
    retv136 = t137
    return retv136
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t141 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t141)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t144 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t144)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    var retv147 int32
    var t148 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(value__2)
    retv147 = t148
    return retv147
}

func _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(self__3 Any) string {
    var retv150 string
    retv150 = "marked"
    return retv150
}

func _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(self__3 Any) string {
    var retv152 string
    retv152 = "marked"
    return retv152
}

func _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(self__3 Any) string {
    var retv154 string
    retv154 = "marked"
    return retv154
}

func iterator_map__A_int32__B_int32__I_Counter(iterator__110 Counter, map_fn__111 func(int32) int32) MapIterator__int32__int32__Counter {
    var retv156 MapIterator__int32__int32__Counter
    var t157 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    retv156 = t157
    return retv156
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__112 MapIterator__int32__int32__Counter, predicate__113 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv159 FilterIterator__int32__MapIterator__int32__int32__Counter
    var t160 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__112,
        predicate: predicate__113,
    }
    retv159 = t160
    return retv159
}

func _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(iterator__114 FilterIterator__int32__MapIterator__int32__int32__Counter, count__115 int) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv162 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var t167 bool = count__115 > 0
    var jp164 int
    if t167 {
        jp164 = count__115
    } else {
        jp164 = 0
    }
    var remaining__116 int = jp164
    var t165 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(remaining__116)
    var t166 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__114,
        remaining: t165,
    }
    retv162 = t166
    return retv162
}

func _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(self__109 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv169 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    retv169 = self__109
    return retv169
}

func _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(self__107 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv171 Option__int32
    var t172 *ref_int_x = self__107.remaining
    var remaining__108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t172)
    var t175 bool = remaining__108 > 0
    var jp174 Option__int32
    if t175 {
        var t176 *ref_int_x = self__107.remaining
        var t177 int = remaining__108 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t176, t177)
        var t178 FilterIterator__int32__MapIterator__int32__int32__Counter = self__107.iterator
        var t179 Option__int32 = _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(t178)
        jp174 = t179
    } else {
        jp174 = Option__int32_None{}
    }
    retv171 = jp174
    return retv171
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__117 FnIterator__int, initial__118 int, combine__119 func(int, int) int) int {
    var retv181 int
    var accumulator__120 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(initial__118)
    var running__121 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop184:
    for {
        var t185 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__121)
        if t185 {
            var mtmp26 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(iterator__117)
            switch mtmp26.(type) {
            case Option__int_None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__121, false)
            case Option__int_Some:
                var x27 int = mtmp26.(Option__int_Some)._0
                var value__122 int = x27
                var t188 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
                var t189 int = combine__119(t188, value__122)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(accumulator__120, t189)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop184
        }
    }
    var t183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
    retv181 = t183
    return retv181
}

func _goml_m_range(start__224 int, end__225 int) FnIterator__int {
    var retv192 FnIterator__int
    var t193 FnIterator__int = __goml_builtin_range(start__224, end__225)
    retv192 = t193
    return retv192
}

func println__T_int(value__1 int) struct{} {
    var t195 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(iterator__123 MapIterator__int__string__FnIterator__int) *_goml_vec_string {
    var retv198 *_goml_vec_string
    var values__124 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var running__125 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop200:
    for {
        var t201 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__125)
        if t201 {
            var mtmp30 Option__string = _goml_m_trait__impl_i_Iterator_h0071ac0c0c2586bdcfed3250d498e716_r____int_i_next(iterator__123)
            switch mtmp30.(type) {
            case Option__string_None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__125, false)
            case Option__string_Some:
                var x31 string = mtmp30.(Option__string_Some)._0
                var value__126 string = x31
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__124, value__126)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop200
        }
    }
    retv198 = values__124
    return retv198
}

func _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(iterator__110 FnIterator__int, map_fn__111 func(int) string) MapIterator__int__string__FnIterator__int {
    var retv206 MapIterator__int__string__FnIterator__int
    var t207 MapIterator__int__string__FnIterator__int = MapIterator__int__string__FnIterator__int{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    retv206 = t207
    return retv206
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv209 string
    var t210 string = _goml_runtime_core_int_to_string(self__5)
    retv209 = t210
    return retv209
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_string_r__i_into__iter(self__185 *_goml_vec_string) FnIterator__string {
    var retv212 FnIterator__string
    var t213 FnIterator__string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__185)
    retv212 = t213
    return retv212
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____string_i_next(self__102 FnIterator__string) Option__string {
    var retv215 Option__string
    var t216 func() Option__string = self__102.next_fn
    var t217 Option__string = t216()
    retv215 = t217
    return retv215
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv219 string
    var t220 string = _goml_runtime_core_int32_to_string(self__43)
    retv219 = t220
    return retv219
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv222 string
    retv222 = self__38
    return retv222
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv224 *ref_int_x
    var t225 *ref_int_x = ref__Ref_3int(value__209)
    retv224 = t225
    return retv224
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv227 int
    var t228 int = ref_get__Ref_3int(self__210)
    retv227 = t228
    return retv227
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(self__105 FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv232 Option__int32
    Loop_loop234:
    for {
        if true {
            var t235 MapIterator__int32__int32__Counter = self__105.iterator
            var mtmp21 Option__int32 = _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(t235)
            switch mtmp21.(type) {
            case Option__int32_None:
                retv232 = Option__int32_None{}
                return retv232
            case Option__int32_Some:
                var x22 int32 = mtmp21.(Option__int32_Some)._0
                var value__106 int32 = x22
                var t238 func(int32) bool = self__105.predicate
                var t239 bool = t238(value__106)
                if t239 {
                    var t240 Option__int32 = Option__int32_Some{
                        _0: value__106,
                    }
                    retv232 = t240
                    return retv232
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop234
        }
    }
    retv232 = Option__int32_None{}
    return retv232
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__209 bool) *ref_bool_x {
    var retv242 *ref_bool_x
    var t243 *ref_bool_x = ref__Ref_4bool(value__209)
    retv242 = t243
    return retv242
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__210 *ref_bool_x) bool {
    var retv245 bool
    var t246 bool = ref_get__Ref_4bool(self__210)
    retv245 = t246
    return retv245
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv248 Option__int
    var t249 func() Option__int = self__102.next_fn
    var t250 Option__int = t249()
    retv248 = t250
    return retv248
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__211 *ref_bool_x, value__212 bool) struct{} {
    ref_set__Ref_4bool(self__211, value__212)
    return struct{}{}
}

func __goml_builtin_range(start__220 int, end__221 int) FnIterator__int {
    var retv254 FnIterator__int
    var current__222 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__220)
    var t255 closure_env_goml_builtin_range_4 = closure_env_goml_builtin_range_4{
        current_0: current__222,
        end_1: end__221,
    }
    var t256 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(t255)
    })
    retv254 = t256
    return retv254
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv258 string
    var t259 string = _goml_runtime_core_int_to_string(self__40)
    retv258 = t259
    return retv258
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv261 *_goml_vec_string
    var t262 *_goml_vec_string = vec_new__Vec_6string()
    retv261 = t262
    return retv261
}

func _goml_m_trait__impl_i_Iterator_h0071ac0c0c2586bdcfed3250d498e716_r____int_i_next(self__103 MapIterator__int__string__FnIterator__int) Option__string {
    var retv264 Option__string
    var t265 FnIterator__int = self__103.iterator
    var mtmp19 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(t265)
    var jp267 Option__string
    switch mtmp19.(type) {
    case Option__int_None:
        jp267 = Option__string_None{}
    case Option__int_Some:
        var x20 int = mtmp19.(Option__int_Some)._0
        var value__104 int = x20
        var t268 func(int) string = self__103.map_fn
        var t269 string = t268(value__104)
        var t270 Option__string = Option__string_Some{
            _0: t269,
        }
        jp267 = t270
    default:
        panic("non-exhaustive match")
    }
    retv264 = jp267
    return retv264
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__128 *_goml_vec_string, elem__129 string) struct{} {
    vec_push__Vec_6string(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__string(self__180 *_goml_vec_string) FnIterator__string {
    var retv274 FnIterator__string
    var index__181 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__182 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__180)
    var t275 closure_env_inherent_Vec_Vec_T_iter_T_string_5 = closure_env_inherent_Vec_Vec_T_iter_T_string_5{
        index_0: index__181,
        len_1: len__182,
        self_2: self__180,
    }
    var t276 FnIterator__string = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(func() Option__string {
        return _goml_m_inherent_i_closure__en_h08b6bd77817c533c5e285632edcbf64d_ring__5_i_apply(t275)
    })
    retv274 = t276
    return retv274
}

func _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(self__103 MapIterator__int32__int32__Counter) Option__int32 {
    var retv278 Option__int32
    var t279 Counter = self__103.iterator
    var mtmp19 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Counter_i_next(t279)
    var jp281 Option__int32
    switch mtmp19.(type) {
    case Option__int32_None:
        jp281 = Option__int32_None{}
    case Option__int32_Some:
        var x20 int32 = mtmp19.(Option__int32_Some)._0
        var value__104 int32 = x20
        var t282 func(int32) int32 = self__103.map_fn
        var t283 int32 = t282(value__104)
        var t284 Option__int32 = Option__int32_Some{
            _0: t283,
        }
        jp281 = t284
    default:
        panic("non-exhaustive match")
    }
    retv278 = jp281
    return retv278
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv286 FnIterator__int
    var t287 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv286 = t287
    return retv286
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__139 *_goml_vec_string) int {
    var retv289 int
    var t290 int = vec_len__Vec_6string(self__139)
    retv289 = t290
    return retv289
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__134 *_goml_vec_string, index__135 int) string {
    var retv292 string
    var t293 string = vec_get__Vec_6string(self__134, index__135)
    retv292 = t293
    return retv292
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__string(next_fn__101 func() Option__string) FnIterator__string {
    var retv295 FnIterator__string
    var t296 FnIterator__string = FnIterator__string{
        next_fn: next_fn__101,
    }
    retv295 = t296
    return retv295
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env82 closure_env_main_0, value__9 int32) int32 {
    var retv317 int32
    var t318 int32 = value__9 * 2
    retv317 = t318
    return retv317
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env83 closure_env_main_1, value__11 int32) bool {
    var retv320 bool
    var t321 bool = value__11 > 4
    retv320 = t321
    return retv320
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env84 closure_env_main_2, total__15 int, value__16 int) int {
    var retv323 int
    var t324 int = total__15 + value__16
    retv323 = t324
    return retv323
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env85 closure_env_main_3, value__18 int) string {
    var retv326 string
    var t327 string = _goml_m_inherent_i_int_i_int_i_to__string(value__18)
    var t328 string = "v" + t327
    retv326 = t328
    return retv326
}

func _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(env86 closure_env_goml_builtin_range_4) Option__int {
    var retv330 Option__int
    var current__222 *ref_int_x = env86.current_0
    var end__221 int = env86.end_1
    var value__223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__222)
    var t333 bool = value__223 < end__221
    var jp332 Option__int
    if t333 {
        var t334 int = value__223 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__222, t334)
        var t335 Option__int = Option__int_Some{
            _0: value__223,
        }
        jp332 = t335
    } else {
        jp332 = Option__int_None{}
    }
    retv330 = jp332
    return retv330
}

func _goml_m_inherent_i_closure__en_h08b6bd77817c533c5e285632edcbf64d_ring__5_i_apply(env87 closure_env_inherent_Vec_Vec_T_iter_T_string_5) Option__string {
    var retv337 Option__string
    var index__181 *ref_int_x = env87.index_0
    var len__182 int = env87.len_1
    var self__180 *_goml_vec_string = env87.self_2
    var current__183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__181)
    var t340 bool = current__183 < len__182
    var jp339 Option__string
    if t340 {
        var value__184 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__180, current__183)
        var t341 int = current__183 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__181, t341)
        var t342 Option__string = Option__string_Some{
            _0: value__184,
        }
        jp339 = t342
    } else {
        jp339 = Option__string_None{}
    }
    retv337 = jp339
    return retv337
}

func main() {
    main0()
}
