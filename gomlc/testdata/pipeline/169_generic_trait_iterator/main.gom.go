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
    var retv90 int32
    retv90 = 7
    return retv90
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    var retv92 string
    retv92 = "seven"
    return retv92
}

func _goml_m_inherent_i_Counter_i_Counter_i_new(start__4 int32, end__5 int32) Counter {
    var retv94 Counter
    var t95 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__4)
    var t96 Counter = Counter{
        current: t95,
        end: end__5,
    }
    retv94 = t96
    return retv94
}

func _goml_m_trait__impl_i_Iterator_i_Counter_i_next(self__6 Counter) Option__int32 {
    var retv98 Option__int32
    var t99 *ref_int32_x = self__6.current
    var current__7 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t99)
    var t102 int32 = self__6.end
    var t103 bool = current__7 < t102
    var jp101 Option__int32
    if t103 {
        var t104 *ref_int32_x = self__6.current
        var t105 int32 = current__7 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t104, t105)
        var t106 Option__int32 = Option__int32_Some{
            _0: current__7,
        }
        jp101 = t106
    } else {
        jp101 = Option__int32_None{}
    }
    retv98 = jp101
    return retv98
}

func main0() struct{} {
    var t108 Token = Token{}
    var t109 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t108)
    println__T_int32(t109)
    var t110 Token = Token{}
    var t111 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t110)
    println__T_string(t111)
    var t112 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t112)
    println__T_int32(converted__8)
    var t113 Any = Any{}
    var t114 string = _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(t113)
    println__T_string(t114)
    var t115 Any = Any{}
    var t116 string = _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(t115)
    println__T_string(t116)
    var t117 Any = Any{}
    var t118 string = _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(t117)
    println__T_string(t118)
    var t119 Counter = _goml_m_inherent_i_Counter_i_Counter_i_new(0, 8)
    var t120 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 MapIterator__int32__int32__Counter = iterator_map__A_int32__B_int32__I_Counter(t119, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t120, p0)
    })
    var t121 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t121, p0)
    })
    var limited__13 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(filtered__12, 3)
    var for_iter71 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(limited__13)
    Loop_loop132:
    for {
        if true {
            var for_next72 Option__int32 = _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(for_iter71)
            switch for_next72.(type) {
            case Option__int32_None:
                break Loop_loop132
            case Option__int32_Some:
                var x73 int32 = for_next72.(Option__int32_Some)._0
                var value__14 int32 = x73
                println__T_int32(value__14)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop132
        }
    }
    var t123 FnIterator__int = _goml_m_range(1, 5)
    var t124 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t123, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t124, p0, p1)
    })
    println__T_int(sum__17)
    var t125 FnIterator__int = _goml_m_range(1, 4)
    var t126 closure_env_main_3 = closure_env_main_3{}
    var t127 MapIterator__int__string__FnIterator__int = _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(t125, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t126, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(t127)
    var for_source77 *_goml_vec_string = texts__19
    var for_limit78 int = vec_len__Vec_6string(for_source77)
    var for_index79 int = 0
    Loop_loop129:
    for {
        var t130 bool = for_index79 < for_limit78
        if t130 {
            var for_item80 string = vec_get__Vec_6string(for_source77, for_index79)
            var t131 int = for_index79 + 1
            for_index79 = t131
            var text__20 string = for_item80
            println__T_string(text__20)
            continue
        } else {
            break Loop_loop129
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv135 *ref_int32_x
    var t136 *ref_int32_x = ref__Ref_5int32(value__209)
    retv135 = t136
    return retv135
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv138 int32
    var t139 int32 = ref_get__Ref_5int32(self__210)
    retv138 = t139
    return retv138
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t143 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t143)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t146 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t146)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    var retv149 int32
    var t150 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(value__2)
    retv149 = t150
    return retv149
}

func _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(self__3 Any) string {
    var retv152 string
    retv152 = "marked"
    return retv152
}

func _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(self__3 Any) string {
    var retv154 string
    retv154 = "marked"
    return retv154
}

func _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(self__3 Any) string {
    var retv156 string
    retv156 = "marked"
    return retv156
}

func iterator_map__A_int32__B_int32__I_Counter(iterator__110 Counter, map_fn__111 func(int32) int32) MapIterator__int32__int32__Counter {
    var retv158 MapIterator__int32__int32__Counter
    var t159 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    retv158 = t159
    return retv158
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__112 MapIterator__int32__int32__Counter, predicate__113 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv161 FilterIterator__int32__MapIterator__int32__int32__Counter
    var t162 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__112,
        predicate: predicate__113,
    }
    retv161 = t162
    return retv161
}

func _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(iterator__114 FilterIterator__int32__MapIterator__int32__int32__Counter, count__115 int) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv164 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var t169 bool = count__115 > 0
    var jp166 int
    if t169 {
        jp166 = count__115
    } else {
        jp166 = 0
    }
    var remaining__116 int = jp166
    var t167 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(remaining__116)
    var t168 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__114,
        remaining: t167,
    }
    retv164 = t168
    return retv164
}

func _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(self__109 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv171 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    retv171 = self__109
    return retv171
}

func _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(self__107 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv173 Option__int32
    var t174 *ref_int_x = self__107.remaining
    var remaining__108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t174)
    var t177 bool = remaining__108 > 0
    var jp176 Option__int32
    if t177 {
        var t178 *ref_int_x = self__107.remaining
        var t179 int = remaining__108 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t178, t179)
        var t180 FilterIterator__int32__MapIterator__int32__int32__Counter = self__107.iterator
        var t181 Option__int32 = _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(t180)
        jp176 = t181
    } else {
        jp176 = Option__int32_None{}
    }
    retv173 = jp176
    return retv173
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__117 FnIterator__int, initial__118 int, combine__119 func(int, int) int) int {
    var retv183 int
    var accumulator__120 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(initial__118)
    var running__121 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop186:
    for {
        var t187 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__121)
        if t187 {
            var mtmp26 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(iterator__117)
            switch mtmp26.(type) {
            case Option__int_None:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__121, false)
            case Option__int_Some:
                var x27 int = mtmp26.(Option__int_Some)._0
                var value__122 int = x27
                var t190 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
                var t191 int = combine__119(t190, value__122)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(accumulator__120, t191)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop186
        }
    }
    var t185 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
    retv183 = t185
    return retv183
}

func _goml_m_range(start__224 int, end__225 int) FnIterator__int {
    var retv194 FnIterator__int
    var t195 FnIterator__int = __goml_builtin_range(start__224, end__225)
    retv194 = t195
    return retv194
}

func println__T_int(value__1 int) struct{} {
    var t197 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t197)
    return struct{}{}
}

func _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(iterator__123 MapIterator__int__string__FnIterator__int) *_goml_vec_string {
    var retv200 *_goml_vec_string
    var values__124 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var running__125 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop202:
    for {
        var t203 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__125)
        if t203 {
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
            break Loop_loop202
        }
    }
    retv200 = values__124
    return retv200
}

func _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(iterator__110 FnIterator__int, map_fn__111 func(int) string) MapIterator__int__string__FnIterator__int {
    var retv208 MapIterator__int__string__FnIterator__int
    var t209 MapIterator__int__string__FnIterator__int = MapIterator__int__string__FnIterator__int{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    retv208 = t209
    return retv208
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv211 string
    var t212 string = _goml_runtime_core_int_to_string(self__5)
    retv211 = t212
    return retv211
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv214 string
    var t215 string = _goml_runtime_core_int32_to_string(self__43)
    retv214 = t215
    return retv214
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv217 string
    retv217 = self__38
    return retv217
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv219 *ref_int_x
    var t220 *ref_int_x = ref__Ref_3int(value__209)
    retv219 = t220
    return retv219
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv222 int
    var t223 int = ref_get__Ref_3int(self__210)
    retv222 = t223
    return retv222
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(self__105 FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv227 Option__int32
    Loop_loop229:
    for {
        if true {
            var t230 MapIterator__int32__int32__Counter = self__105.iterator
            var mtmp21 Option__int32 = _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(t230)
            switch mtmp21.(type) {
            case Option__int32_None:
                retv227 = Option__int32_None{}
                return retv227
            case Option__int32_Some:
                var x22 int32 = mtmp21.(Option__int32_Some)._0
                var value__106 int32 = x22
                var t233 func(int32) bool = self__105.predicate
                var t234 bool = t233(value__106)
                if t234 {
                    var t235 Option__int32 = Option__int32_Some{
                        _0: value__106,
                    }
                    retv227 = t235
                    return retv227
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop229
        }
    }
    retv227 = Option__int32_None{}
    return retv227
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__209 bool) *ref_bool_x {
    var retv237 *ref_bool_x
    var t238 *ref_bool_x = ref__Ref_4bool(value__209)
    retv237 = t238
    return retv237
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__210 *ref_bool_x) bool {
    var retv240 bool
    var t241 bool = ref_get__Ref_4bool(self__210)
    retv240 = t241
    return retv240
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv243 Option__int
    var t244 func() Option__int = self__102.next_fn
    var t245 Option__int = t244()
    retv243 = t245
    return retv243
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__211 *ref_bool_x, value__212 bool) struct{} {
    ref_set__Ref_4bool(self__211, value__212)
    return struct{}{}
}

func __goml_builtin_range(start__220 int, end__221 int) FnIterator__int {
    var retv249 FnIterator__int
    var current__222 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__220)
    var t250 closure_env_goml_builtin_range_4 = closure_env_goml_builtin_range_4{
        current_0: current__222,
        end_1: end__221,
    }
    var t251 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(t250)
    })
    retv249 = t251
    return retv249
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv253 string
    var t254 string = _goml_runtime_core_int_to_string(self__40)
    retv253 = t254
    return retv253
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv256 *_goml_vec_string
    var t257 *_goml_vec_string = vec_new__Vec_6string()
    retv256 = t257
    return retv256
}

func _goml_m_trait__impl_i_Iterator_h0071ac0c0c2586bdcfed3250d498e716_r____int_i_next(self__103 MapIterator__int__string__FnIterator__int) Option__string {
    var retv259 Option__string
    var t260 FnIterator__int = self__103.iterator
    var mtmp19 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(t260)
    var jp262 Option__string
    switch mtmp19.(type) {
    case Option__int_None:
        jp262 = Option__string_None{}
    case Option__int_Some:
        var x20 int = mtmp19.(Option__int_Some)._0
        var value__104 int = x20
        var t263 func(int) string = self__103.map_fn
        var t264 string = t263(value__104)
        var t265 Option__string = Option__string_Some{
            _0: t264,
        }
        jp262 = t265
    default:
        panic("non-exhaustive match")
    }
    retv259 = jp262
    return retv259
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__128 *_goml_vec_string, elem__129 string) struct{} {
    vec_push__Vec_6string(self__128, elem__129)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(self__103 MapIterator__int32__int32__Counter) Option__int32 {
    var retv269 Option__int32
    var t270 Counter = self__103.iterator
    var mtmp19 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Counter_i_next(t270)
    var jp272 Option__int32
    switch mtmp19.(type) {
    case Option__int32_None:
        jp272 = Option__int32_None{}
    case Option__int32_Some:
        var x20 int32 = mtmp19.(Option__int32_Some)._0
        var value__104 int32 = x20
        var t273 func(int32) int32 = self__103.map_fn
        var t274 int32 = t273(value__104)
        var t275 Option__int32 = Option__int32_Some{
            _0: t274,
        }
        jp272 = t275
    default:
        panic("non-exhaustive match")
    }
    retv269 = jp272
    return retv269
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv277 FnIterator__int
    var t278 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv277 = t278
    return retv277
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env84 closure_env_main_0, value__9 int32) int32 {
    var retv299 int32
    var t300 int32 = value__9 * 2
    retv299 = t300
    return retv299
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env85 closure_env_main_1, value__11 int32) bool {
    var retv302 bool
    var t303 bool = value__11 > 4
    retv302 = t303
    return retv302
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env86 closure_env_main_2, total__15 int, value__16 int) int {
    var retv305 int
    var t306 int = total__15 + value__16
    retv305 = t306
    return retv305
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env87 closure_env_main_3, value__18 int) string {
    var retv308 string
    var t309 string = _goml_m_inherent_i_int_i_int_i_to__string(value__18)
    var t310 string = "v" + t309
    retv308 = t310
    return retv308
}

func _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(env88 closure_env_goml_builtin_range_4) Option__int {
    var retv312 Option__int
    var current__222 *ref_int_x = env88.current_0
    var end__221 int = env88.end_1
    var value__223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__222)
    var t315 bool = value__223 < end__221
    var jp314 Option__int
    if t315 {
        var t316 int = value__223 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__222, t316)
        var t317 Option__int = Option__int_Some{
            _0: value__223,
        }
        jp314 = t317
    } else {
        jp314 = Option__int_None{}
    }
    retv312 = jp314
    return retv312
}

func main() {
    main0()
}
