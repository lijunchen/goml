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

type Numbers struct {
    values *_goml_vec_int32
    conversions *ref_int32_x
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_0 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_int32
}

type closure_env_inherent_Slice_Slice_T_iter_T_int32_1 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 []int32
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func _goml_m_trait__impl_i_IntoIterator_i_Numbers_i_into__iter(self__0 Numbers) FnIterator__int32 {
    var retv173 FnIterator__int32
    var t174 *ref_int32_x = self__0.conversions
    var t175 *ref_int32_x = self__0.conversions
    var t176 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t175)
    var t177 int32 = t176 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t174, t177)
    var t178 *_goml_vec_int32 = self__0.values
    var t179 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t178)
    retv173 = t179
    return retv173
}

func make_numbers(builds__1 *ref_int32_x, conversions__2 *ref_int32_x) Numbers {
    var retv181 Numbers
    var t182 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__1)
    var t183 int32 = t182 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__1, t183)
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 3)
    var t184 Numbers = Numbers{
        values: values__3,
        conversions: conversions__2,
    }
    retv181 = t184
    return retv181
}

func main0() struct{} {
    var builds__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var conversions__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t186 Numbers = make_numbers(builds__7, conversions__8)
    var t187 int32 = sum__S_Numbers(t186)
    println__T_int32(t187)
    var t188 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    println__T_int32(t188)
    var t189 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(conversions__8)
    println__T_int32(t189)
    var values__9 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 30)
    var t190 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    println__T_int32(t190)
    var t191 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__9, 1, 3)
    var t192 int32 = _goml_m_sum____S__Slice_l_int32_r_(t191)
    println__T_int32(t192)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv194 int32
    var t195 int32 = ref_get__Ref_5int32(self__208)
    retv194 = t195
    return retv194
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__178 *_goml_vec_int32) FnIterator__int32 {
    var retv199 FnIterator__int32
    var index__179 *ref_int_x = ref__Ref_3int(0)
    var len__180 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__178)
    var t200 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__179,
        len_1: len__180,
        self_2: self__178,
    }
    var t201 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t200)
    })
    retv199 = t201
    return retv199
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv203 *_goml_vec_int32
    var t204 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv203 = t204
    return retv203
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv208 *ref_int32_x
    var t209 *ref_int32_x = ref__Ref_5int32(value__207)
    retv208 = t209
    return retv208
}

func println__T_int32(value__1 int32) struct{} {
    var t211 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t211)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var retv214 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter157 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Numbers_i_into__iter(source__4)
    Loop_loop217:
    for {
        if true {
            var for_next158 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter157)
            switch for_next158.(type) {
            case None:
                break Loop_loop217
            case Some:
                var x159 int32 = for_next158.(Some)._0
                var value__6 int32 = x159
                var t219 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t220 int32 = t219 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t220)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop217
        }
    }
    var t216 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv214 = t216
    return retv214
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var retv222 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter157 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(source__4)
    Loop_loop225:
    for {
        if true {
            var for_next158 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter157)
            switch for_next158.(type) {
            case None:
                break Loop_loop225
            case Some:
                var x159 int32 = for_next158.(Some)._0
                var value__6 int32 = x159
                var t227 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t228 int32 = t227 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t228)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop225
        }
    }
    var t224 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv222 = t224
    return retv222
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var retv230 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter157 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(source__4)
    Loop_loop233:
    for {
        if true {
            var for_next158 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter157)
            switch for_next158.(type) {
            case None:
                break Loop_loop233
            case Some:
                var x159 int32 = for_next158.(Some)._0
                var value__6 int32 = x159
                var t235 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t236 int32 = t235 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t236)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop233
        }
    }
    var t232 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv230 = t232
    return retv230
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var retv238 []int32
    var t239 []int32 = self__175.items[start__176:end__177]
    retv238 = t239
    return retv238
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv241 int
    var t242 int = vec_len__Vec_5int32(self__137)
    retv241 = t242
    return retv241
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv244 FnIterator__int32
    var t245 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv244 = t245
    return retv244
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv247 string
    var t248 string = _goml_runtime_core_int32_to_string(self__43)
    retv247 = t248
    return retv247
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv250 Option__int32
    var t251 func() Option__int32 = self__102.next_fn
    var t252 Option__int32 = t251()
    retv250 = t252
    return retv250
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__183 *_goml_vec_int32) FnIterator__int32 {
    var retv254 FnIterator__int32
    var t255 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__183)
    retv254 = t255
    return retv254
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__195 []int32) FnIterator__int32 {
    var retv257 FnIterator__int32
    var t258 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__195)
    retv257 = t258
    return retv257
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__190 []int32) FnIterator__int32 {
    var retv260 FnIterator__int32
    var index__191 *ref_int_x = ref__Ref_3int(0)
    var len__192 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__190)
    var t261 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__191,
        len_1: len__192,
        self_2: self__190,
    }
    var t262 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t261)
    })
    retv260 = t262
    return retv260
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__186 []int32) int {
    var retv264 int
    var t265 int = len(self__186)
    retv264 = t265
    return retv264
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__184 []int32, index__185 int) int32 {
    var retv267 int32
    var t268 int32 = self__184[index__185]
    retv267 = t268
    return retv267
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env170 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv282 Option__int32
    var index__179 *ref_int_x = env170.index_0
    var len__180 int = env170.len_1
    var self__178 *_goml_vec_int32 = env170.self_2
    var current__181 int = ref_get__Ref_3int(index__179)
    var t285 bool = current__181 < len__180
    var jp284 Option__int32
    if t285 {
        var value__182 int32 = vec_get__Vec_5int32(self__178, current__181)
        var t286 int = current__181 + 1
        ref_set__Ref_3int(index__179, t286)
        var t287 Option__int32 = Some{
            _0: value__182,
        }
        jp284 = t287
    } else {
        jp284 = None{}
    }
    retv282 = jp284
    return retv282
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env171 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var retv289 Option__int32
    var index__191 *ref_int_x = env171.index_0
    var len__192 int = env171.len_1
    var self__190 []int32 = env171.self_2
    var current__193 int = ref_get__Ref_3int(index__191)
    var t292 bool = current__193 < len__192
    var jp291 Option__int32
    if t292 {
        var value__194 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__190, current__193)
        var t293 int = current__193 + 1
        ref_set__Ref_3int(index__191, t293)
        var t294 Option__int32 = Some{
            _0: value__194,
        }
        jp291 = t294
    } else {
        jp291 = None{}
    }
    retv289 = jp291
    return retv289
}

func main() {
    main0()
}
