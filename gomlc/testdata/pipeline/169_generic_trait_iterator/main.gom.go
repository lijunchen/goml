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
    var retv94 int32
    retv94 = 7
    return retv94
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(self__1 Token) string {
    var retv96 string
    retv96 = "seven"
    return retv96
}

func _goml_m_inherent_i_Counter_i_Counter_i_new(start__4 int32, end__5 int32) Counter {
    var retv98 Counter
    var t99 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__4)
    var t100 Counter = Counter{
        current: t99,
        end: end__5,
    }
    retv98 = t100
    return retv98
}

func _goml_m_trait__impl_i_Iterator_i_Counter_i_next(self__6 Counter) Option__int32 {
    var retv102 Option__int32
    var t103 *ref_int32_x = self__6.current
    var current__7 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t103)
    var t106 int32 = self__6.end
    var t107 bool = current__7 < t106
    var jp105 Option__int32
    if t107 {
        var t108 *ref_int32_x = self__6.current
        var t109 int32 = current__7 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t108, t109)
        var t110 Option__int32 = Option__int32_Some{
            _0: current__7,
        }
        jp105 = t110
    } else {
        jp105 = Option__int32_None{}
    }
    retv102 = jp105
    return retv102
}

func main0() struct{} {
    var t112 Token = Token{}
    var t113 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(t112)
    println__T_int32(t113)
    var t114 Token = Token{}
    var t115 string = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Token_i_convert(t114)
    println__T_string(t115)
    var t116 Token = Token{}
    var converted__8 int32 = convert_to__T_int32__V_Token(t116)
    println__T_int32(converted__8)
    var t117 Any = Any{}
    var t118 string = _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(t117)
    println__T_string(t118)
    var t119 Any = Any{}
    var t120 string = _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(t119)
    println__T_string(t120)
    var t121 Any = Any{}
    var t122 string = _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(t121)
    println__T_string(t122)
    var t123 Counter = _goml_m_inherent_i_Counter_i_Counter_i_new(0, 8)
    var t124 closure_env_main_0 = closure_env_main_0{}
    var mapped__10 MapIterator__int32__int32__Counter = iterator_map__A_int32__B_int32__I_Counter(t123, func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t124, p0)
    })
    var t125 closure_env_main_1 = closure_env_main_1{}
    var filtered__12 FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(mapped__10, func(p0 int32) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t125, p0)
    })
    var limited__13 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(filtered__12, 3)
    var for_iter75 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(limited__13)
    Loop_loop136:
    for {
        if true {
            var for_next76 Option__int32 = _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(for_iter75)
            switch for_next76.(type) {
            case Option__int32_None:
                break Loop_loop136
            case Option__int32_Some:
                var x77 int32 = for_next76.(Option__int32_Some)._0
                var value__14 int32 = x77
                println__T_int32(value__14)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop136
        }
    }
    var t127 FnIterator__int = _goml_m_range(1, 5)
    var t128 closure_env_main_2 = closure_env_main_2{}
    var sum__17 int = _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(t127, 0, func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t128, p0, p1)
    })
    println__T_int(sum__17)
    var t129 FnIterator__int = _goml_m_range(1, 4)
    var t130 closure_env_main_3 = closure_env_main_3{}
    var t131 MapIterator__int__string__FnIterator__int = _goml_m_iterator__map____A__int____B__string____I__FnIterator_l_int_r_(t129, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t130, p0)
    })
    var texts__19 *_goml_vec_string = _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(t131)
    var for_source81 *_goml_vec_string = texts__19
    var for_limit82 int = vec_len__Vec_6string(for_source81)
    var for_index83 int = 0
    Loop_loop133:
    for {
        var t134 bool = for_index83 < for_limit82
        if t134 {
            var for_item84 string = vec_get__Vec_6string(for_source81, for_index83)
            var t135 int = for_index83 + 1
            for_index83 = t135
            var text__20 string = for_item84
            println__T_string(text__20)
            continue
        } else {
            break Loop_loop133
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv139 *ref_int32_x
    var t140 *ref_int32_x = ref__Ref_5int32(value__207)
    retv139 = t140
    return retv139
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv142 int32
    var t143 int32 = ref_get__Ref_5int32(self__208)
    retv142 = t143
    return retv142
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t147 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t147)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t150 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t150)
    return struct{}{}
}

func convert_to__T_int32__V_Token(value__2 Token) int32 {
    var retv153 int32
    var t154 int32 = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Token_i_convert(value__2)
    retv153 = t154
    return retv153
}

func _goml_m_trait__impl_i_Marker_i__l_int32_r__x40_Any_i_marker(self__3 Any) string {
    var retv156 string
    retv156 = "marked"
    return retv156
}

func _goml_m_trait__impl_i_Marker_i__l_string_r__x40_Any_i_marker(self__3 Any) string {
    var retv158 string
    retv158 = "marked"
    return retv158
}

func _goml_m_trait__impl_i_Marker_i__l_Vec_l_int32_r__r__x40_Any_i_marker(self__3 Any) string {
    var retv160 string
    retv160 = "marked"
    return retv160
}

func iterator_map__A_int32__B_int32__I_Counter(iterator__110 Counter, map_fn__111 func(int32) int32) MapIterator__int32__int32__Counter {
    var retv162 MapIterator__int32__int32__Counter
    var t163 MapIterator__int32__int32__Counter = MapIterator__int32__int32__Counter{
        iterator: iterator__110,
        map_fn: map_fn__111,
    }
    retv162 = t163
    return retv162
}

func _goml_m_iterator__filter____I__hae120c2dac596b59fedf4cc0625830b9__r_____T__int32(iterator__112 MapIterator__int32__int32__Counter, predicate__113 func(int32) bool) FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv165 FilterIterator__int32__MapIterator__int32__int32__Counter
    var t166 FilterIterator__int32__MapIterator__int32__int32__Counter = FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__112,
        predicate: predicate__113,
    }
    retv165 = t166
    return retv165
}

func _goml_m_iterator__take____I__F_hedf720f49984423d1d3f538800202cfe_c_Counter_r__r_(iterator__114 FilterIterator__int32__MapIterator__int32__int32__Counter, count__115 int) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv168 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    var t173 bool = count__115 > 0
    var jp170 int
    if t173 {
        jp170 = count__115
    } else {
        jp170 = 0
    }
    var remaining__116 int = jp170
    var t171 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(remaining__116)
    var t172 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter = TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter{
        iterator: iterator__114,
        remaining: t171,
    }
    retv168 = t172
    return retv168
}

func _goml_m_trait__impl_i_IntoIter_h4574c0f9c79f14d6a78002fe187a9106_er_i_into__iter(self__109 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter {
    var retv175 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter
    retv175 = self__109
    return retv175
}

func _goml_m_trait__impl_i_Iterator_h1d781e9b352eff9defa895ed47b740ba__Counter_i_next(self__107 TakeIterator__FilterIterator__int32__MapIterator__int32__int32__Counter) Option__int32 {
    var retv177 Option__int32
    var t178 *ref_int_x = self__107.remaining
    var remaining__108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t178)
    var t181 bool = remaining__108 > 0
    var jp180 Option__int32
    if t181 {
        var t182 *ref_int_x = self__107.remaining
        var t183 int = remaining__108 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t182, t183)
        var t184 FilterIterator__int32__MapIterator__int32__int32__Counter = self__107.iterator
        var t185 Option__int32 = _goml_m_trait__impl_i_Iterator_h6f309c390d4008e9eda9489200973074__Counter_i_next(t184)
        jp180 = t185
    } else {
        jp180 = Option__int32_None{}
    }
    retv177 = jp180
    return retv177
}

func _goml_m_iterator__fold____A__int____I__FnIterator_l_int_r_____T__int(iterator__117 FnIterator__int, initial__118 int, combine__119 func(int, int) int) int {
    var retv187 int
    var accumulator__120 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(initial__118)
    Loop_loop190:
    for {
        if true {
            var mtmp26 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(iterator__117)
            switch mtmp26.(type) {
            case Option__int_None:
                break Loop_loop190
            case Option__int_Some:
                var x27 int = mtmp26.(Option__int_Some)._0
                var value__121 int = x27
                var t192 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
                var t193 int = combine__119(t192, value__121)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(accumulator__120, t193)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop190
        }
    }
    var t189 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(accumulator__120)
    retv187 = t189
    return retv187
}

func _goml_m_range(start__222 int, end__223 int) FnIterator__int {
    var retv196 FnIterator__int
    var t197 FnIterator__int = __goml_builtin_range(start__222, end__223)
    retv196 = t197
    return retv196
}

func println__T_int(value__1 int) struct{} {
    var t199 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t199)
    return struct{}{}
}

func _goml_m_iterator__collect____I_h71545e56394faca5741d280b5e9d3d51_r_____T__string(iterator__122 MapIterator__int__string__FnIterator__int) *_goml_vec_string {
    var retv202 *_goml_vec_string
    var values__123 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    Loop_loop204:
    for {
        if true {
            var mtmp30 Option__string = _goml_m_trait__impl_i_Iterator_h0071ac0c0c2586bdcfed3250d498e716_r____int_i_next(iterator__122)
            switch mtmp30.(type) {
            case Option__string_None:
                break Loop_loop204
            case Option__string_Some:
                var x31 string = mtmp30.(Option__string_Some)._0
                var value__124 string = x31
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__123, value__124)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop204
        }
    }
    retv202 = values__123
    return retv202
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv219 *ref_int_x
    var t220 *ref_int_x = ref__Ref_3int(value__207)
    retv219 = t220
    return retv219
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv222 int
    var t223 int = ref_get__Ref_3int(self__208)
    retv222 = t223
    return retv222
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
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

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv237 Option__int
    var t238 func() Option__int = self__102.next_fn
    var t239 Option__int = t238()
    retv237 = t239
    return retv237
}

func __goml_builtin_range(start__218 int, end__219 int) FnIterator__int {
    var retv241 FnIterator__int
    var current__220 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__218)
    var t242 closure_env_goml_builtin_range_4 = closure_env_goml_builtin_range_4{
        current_0: current__220,
        end_1: end__219,
    }
    var t243 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(t242)
    })
    retv241 = t243
    return retv241
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv245 string
    var t246 string = _goml_runtime_core_int_to_string(self__40)
    retv245 = t246
    return retv245
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv248 *_goml_vec_string
    var t249 *_goml_vec_string = vec_new__Vec_6string()
    retv248 = t249
    return retv248
}

func _goml_m_trait__impl_i_Iterator_h0071ac0c0c2586bdcfed3250d498e716_r____int_i_next(self__103 MapIterator__int__string__FnIterator__int) Option__string {
    var retv251 Option__string
    var t252 FnIterator__int = self__103.iterator
    var mtmp19 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(t252)
    var jp254 Option__string
    switch mtmp19.(type) {
    case Option__int_None:
        jp254 = Option__string_None{}
    case Option__int_Some:
        var x20 int = mtmp19.(Option__int_Some)._0
        var value__104 int = x20
        var t255 func(int) string = self__103.map_fn
        var t256 string = t255(value__104)
        var t257 Option__string = Option__string_Some{
            _0: t256,
        }
        jp254 = t257
    default:
        panic("non-exhaustive match")
    }
    retv251 = jp254
    return retv251
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Iterator_i_MapIterator____int32____int32____Counter_i_next(self__103 MapIterator__int32__int32__Counter) Option__int32 {
    var retv261 Option__int32
    var t262 Counter = self__103.iterator
    var mtmp19 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Counter_i_next(t262)
    var jp264 Option__int32
    switch mtmp19.(type) {
    case Option__int32_None:
        jp264 = Option__int32_None{}
    case Option__int32_Some:
        var x20 int32 = mtmp19.(Option__int32_Some)._0
        var value__104 int32 = x20
        var t265 func(int32) int32 = self__103.map_fn
        var t266 int32 = t265(value__104)
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

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv269 FnIterator__int
    var t270 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv269 = t270
    return retv269
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env88 closure_env_main_0, value__9 int32) int32 {
    var retv284 int32
    var t285 int32 = value__9 * 2
    retv284 = t285
    return retv284
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env89 closure_env_main_1, value__11 int32) bool {
    var retv287 bool
    var t288 bool = value__11 > 4
    retv287 = t288
    return retv287
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env90 closure_env_main_2, total__15 int, value__16 int) int {
    var retv290 int
    var t291 int = total__15 + value__16
    retv290 = t291
    return retv290
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env91 closure_env_main_3, value__18 int) string {
    var retv293 string
    var t294 string = _goml_m_inherent_i_int_i_int_i_to__string(value__18)
    var t295 string = "v" + t294
    retv293 = t295
    return retv293
}

func _goml_m_inherent_i_closure__en_h79ff66493e488e4d6e1521a7bcb9649c_ange__4_i_apply(env92 closure_env_goml_builtin_range_4) Option__int {
    var retv297 Option__int
    var current__220 *ref_int_x = env92.current_0
    var end__219 int = env92.end_1
    var value__221 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__220)
    var t300 bool = value__221 < end__219
    var jp299 Option__int
    if t300 {
        var t301 int = value__221 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__220, t301)
        var t302 Option__int = Option__int_Some{
            _0: value__221,
        }
        jp299 = t302
    } else {
        jp299 = Option__int_None{}
    }
    retv297 = jp299
    return retv297
}

func main() {
    main0()
}
