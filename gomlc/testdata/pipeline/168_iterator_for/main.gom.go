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

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

type _goml_vec_Tuple2_5int32_6string struct {
    items []Tuple2_5int32_6string
}

func vec_new__Vec_21Tuple2_5int32_6string() *_goml_vec_Tuple2_5int32_6string {
    return &_goml_vec_Tuple2_5int32_6string{
        items: nil,
    }
}

func vec_push__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string, elem Tuple2_5int32_6string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string, index int) Tuple2_5int32_6string {
    return vec.items[index]
}

func vec_len__Vec_21Tuple2_5int32_6string(vec *_goml_vec_Tuple2_5int32_6string) int {
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

type Tuple2_5int32_6string struct {
    _0 int32
    _1 string
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_countdown_0 struct {
    current_0 *ref_int32_x
}

type closure_env_goml_builtin_range_1 struct {
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

func countdown(start__0 int32) FnIterator__int32 {
    var retv124 FnIterator__int32
    var current__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__0)
    var t125 closure_env_countdown_0 = closure_env_countdown_0{
        current_0: current__1,
    }
    var t126 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(t125)
    })
    retv124 = t126
    return retv124
}

func counted_range(calls__3 *ref_int32_x) FnIterator__int {
    var retv128 FnIterator__int
    var t129 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__3)
    var t130 int32 = t129 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(calls__3, t130)
    var t131 FnIterator__int = _goml_m_range(1, 5)
    retv128 = t131
    return retv128
}

func first_even(values__4 FnIterator__int) int {
    var retv133 int
    var for_iter66 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(values__4)
    Loop_loop135:
    for {
        if true {
            var for_next67 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter66)
            switch for_next67.(type) {
            case Option__int_None:
                break Loop_loop135
            case Option__int_Some:
                var x68 int = for_next67.(Option__int_Some)._0
                var value__5 int = x68
                var t138 int = value__5 / 2
                var t139 int = t138 * 2
                var t140 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t139, value__5)
                if t140 {
                    retv133 = value__5
                    return retv133
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop135
        }
    }
    retv133 = -1
    return retv133
}

func main0() struct{} {
    var values__6 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__6, 30)
    var sum__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source74 *_goml_vec_int32 = values__6
    var for_limit75 int = vec_len__Vec_5int32(for_source74)
    var for_index76 int = 0
    Loop_loop178:
    for {
        var t179 bool = for_index76 < for_limit75
        if t179 {
            var for_item77 int32 = vec_get__Vec_5int32(for_source74, for_index76)
            var t180 int = for_index76 + 1
            for_index76 = t180
            var value__8 int32 = for_item77
            var t184 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__8, 20)
            if t184 {
                continue
            } else {
                var t182 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
                var t183 int32 = t182 + value__8
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__7, t183)
                continue
            }
        } else {
            break Loop_loop178
        }
    }
    var t143 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__7)
    println__T_int32(t143)
    var pairs__9 *_goml_vec_Tuple2_5int32_6string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_()
    var t144 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "a",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t144)
    var t145 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "b",
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(pairs__9, t145)
    var for_source85 *_goml_vec_Tuple2_5int32_6string = pairs__9
    var for_limit86 int = vec_len__Vec_21Tuple2_5int32_6string(for_source85)
    var for_index87 int = 0
    Loop_loop173:
    for {
        var t174 bool = for_index87 < for_limit86
        if t174 {
            var for_item88 Tuple2_5int32_6string = vec_get__Vec_21Tuple2_5int32_6string(for_source85, for_index87)
            var t175 int = for_index87 + 1
            for_index87 = t175
            var x90 int32 = for_item88._0
            var x91 string = for_item88._1
            var text__11 string = x91
            var number__10 int32 = x90
            var t176 string = _goml_m_inherent_i_int32_i_int32_i_to__string(number__10)
            var t177 string = t176 + text__11
            println__T_string(t177)
            continue
        } else {
            break Loop_loop173
        }
    }
    var calls__12 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var range_sum__13 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t147 FnIterator__int = counted_range(calls__12)
    var for_iter94 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t147)
    Loop_loop169:
    for {
        if true {
            var for_next95 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter94)
            switch for_next95.(type) {
            case Option__int_None:
                break Loop_loop169
            case Option__int_Some:
                var x96 int = for_next95.(Option__int_Some)._0
                var value__14 int = x96
                var t171 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
                var t172 int = t171 + value__14
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(range_sum__13, t172)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop169
        }
    }
    var t149 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(calls__12)
    println__T_int32(t149)
    var t150 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(range_sum__13)
    println__T_int(t150)
    var slice_sum__15 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_source101 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__6, 1, 3)
    var for_limit102 int = len(for_source101)
    var for_index103 int = 0
    Loop_loop164:
    for {
        var t165 bool = for_index103 < for_limit102
        if t165 {
            var for_item104 int32 = for_source101[for_index103]
            var t166 int = for_index103 + 1
            for_index103 = t166
            var value__16 int32 = for_item104
            var t167 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
            var t168 int32 = t167 + value__16
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(slice_sum__15, t168)
            continue
        } else {
            break Loop_loop164
        }
    }
    var t152 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(slice_sum__15)
    println__T_int32(t152)
    var t153 FnIterator__int32 = countdown(4)
    var for_iter109 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t153)
    Loop_loop160:
    for {
        if true {
            var for_next110 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter109)
            switch for_next110.(type) {
            case Option__int32_None:
                break Loop_loop160
            case Option__int32_Some:
                var x111 int32 = for_next110.(Option__int32_Some)._0
                var value__17 int32 = x111
                var t163 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(value__17, 2)
                if t163 {
                    break Loop_loop160
                } else {
                    println__T_int32(value__17)
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop160
        }
    }
    var empty__18 FnIterator__int = _goml_m_range(0, 0)
    var for_iter115 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(empty__18)
    Loop_loop158:
    for {
        if true {
            var for_next116 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter115)
            switch for_next116.(type) {
            case Option__int_None:
                break Loop_loop158
            case Option__int_Some:
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop158
        }
    }
    var t156 FnIterator__int = _goml_m_range(3, 8)
    var t157 int = first_even(t156)
    println__T_int(t157)
    println__T_string("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv186 *ref_int32_x
    var t187 *ref_int32_x = ref__Ref_5int32(value__209)
    retv186 = t187
    return retv186
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv189 int32
    var t190 int32 = ref_get__Ref_5int32(self__210)
    retv189 = t190
    return retv189
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv194 FnIterator__int32
    var t195 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv194 = t195
    return retv194
}

func _goml_m_range(start__224 int, end__225 int) FnIterator__int {
    var retv197 FnIterator__int
    var t198 FnIterator__int = __goml_builtin_range(start__224, end__225)
    retv197 = t198
    return retv197
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    var retv200 FnIterator__int
    retv200 = self__109
    return retv200
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv202 Option__int
    var t203 func() Option__int = self__102.next_fn
    var t204 Option__int = t203()
    retv202 = t204
    return retv202
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv206 bool
    var t207 bool = self__59 == other__60
    retv206 = t207
    return retv206
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv209 *_goml_vec_int32
    var t210 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv209 = t210
    return retv209
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__128 *_goml_vec_int32, elem__129 int32) struct{} {
    vec_push__Vec_5int32(self__128, elem__129)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv214 bool
    var t215 bool = self__65 == other__66
    retv214 = t215
    return retv214
}

func println__T_int32(value__1 int32) struct{} {
    var t217 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t217)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int32_c_string_q_() *_goml_vec_Tuple2_5int32_6string {
    var retv220 *_goml_vec_Tuple2_5int32_6string
    var t221 *_goml_vec_Tuple2_5int32_6string = vec_new__Vec_21Tuple2_5int32_6string()
    retv220 = t221
    return retv220
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int32_c_string_q_(self__128 *_goml_vec_Tuple2_5int32_6string, elem__129 Tuple2_5int32_6string) struct{} {
    vec_push__Vec_21Tuple2_5int32_6string(self__128, elem__129)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t225)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv228 string
    var t229 string = _goml_runtime_core_int32_to_string(self__6)
    retv228 = t229
    return retv228
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv231 *ref_int_x
    var t232 *ref_int_x = ref__Ref_3int(value__209)
    retv231 = t232
    return retv231
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv234 int
    var t235 int = ref_get__Ref_3int(self__210)
    retv234 = t235
    return retv234
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t239 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t239)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__177 *_goml_vec_int32, start__178 int, end__179 int) []int32 {
    var retv242 []int32
    var t243 []int32 = self__177.items[start__178:end__179]
    retv242 = t243
    return retv242
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(self__109 FnIterator__int32) FnIterator__int32 {
    var retv245 FnIterator__int32
    retv245 = self__109
    return retv245
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv247 Option__int32
    var t248 func() Option__int32 = self__102.next_fn
    var t249 Option__int32 = t248()
    retv247 = t249
    return retv247
}

func __goml_builtin_range(start__220 int, end__221 int) FnIterator__int {
    var retv251 FnIterator__int
    var current__222 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__220)
    var t252 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__222,
        end_1: end__221,
    }
    var t253 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t252)
    })
    retv251 = t253
    return retv251
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv255 string
    var t256 string = _goml_runtime_core_int32_to_string(self__43)
    retv255 = t256
    return retv255
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv258 string
    retv258 = self__38
    return retv258
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv260 string
    var t261 string = _goml_runtime_core_int_to_string(self__40)
    retv260 = t261
    return retv260
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv263 FnIterator__int
    var t264 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv263 = t264
    return retv263
}

func _goml_m_inherent_i_closure__en_hc257e8a36c560f54cbc91ed82b2d188c_down__0_i_apply(env121 closure_env_countdown_0) Option__int32 {
    var retv272 Option__int32
    var current__1 *ref_int32_x = env121.current_0
    var value__2 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__1)
    var t275 bool = value__2 > 0
    var jp274 Option__int32
    if t275 {
        var t276 int32 = value__2 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__1, t276)
        var t277 Option__int32 = Option__int32_Some{
            _0: value__2,
        }
        jp274 = t277
    } else {
        jp274 = Option__int32_None{}
    }
    retv272 = jp274
    return retv272
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env122 closure_env_goml_builtin_range_1) Option__int {
    var retv279 Option__int
    var current__222 *ref_int_x = env122.current_0
    var end__221 int = env122.end_1
    var value__223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__222)
    var t282 bool = value__223 < end__221
    var jp281 Option__int
    if t282 {
        var t283 int = value__223 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__222, t283)
        var t284 Option__int = Option__int_Some{
            _0: value__223,
        }
        jp281 = t284
    } else {
        jp281 = Option__int_None{}
    }
    retv279 = jp281
    return retv279
}

func main() {
    main0()
}
