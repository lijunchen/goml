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
    var t177 *ref_int32_x = self__0.conversions
    var t178 *ref_int32_x = self__0.conversions
    var t179 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t178)
    var t180 int32 = t179 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t177, t180)
    var t181 *_goml_vec_int32 = self__0.values
    var t182 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t181)
    return t182
}

func make_numbers(builds__1 *ref_int32_x, conversions__2 *ref_int32_x) Numbers {
    var t185 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__1)
    var t186 int32 = t185 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__1, t186)
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 3)
    var t187 Numbers = Numbers{
        values: values__3,
        conversions: conversions__2,
    }
    return t187
}

func main0() struct{} {
    var builds__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var conversions__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t189 Numbers = make_numbers(builds__7, conversions__8)
    var t190 int32 = sum__S_Numbers(t189)
    println__T_int32(t190)
    var t191 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    println__T_int32(t191)
    var t192 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(conversions__8)
    println__T_int32(t192)
    var values__9 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 30)
    var t193 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    println__T_int32(t193)
    var t194 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__9, 1, 3)
    var t195 int32 = _goml_m_sum____S__Slice_l_int32_r_(t194)
    println__T_int32(t195)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var t198 int32 = ref_get__Ref_5int32(self__208)
    return t198
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__178 *_goml_vec_int32) FnIterator__int32 {
    var index__179 *ref_int_x = ref__Ref_3int(0)
    var len__180 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__178)
    var t203 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__179,
        len_1: len__180,
        self_2: self__178,
    }
    var t204 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t203)
    })
    return t204
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t207 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t207
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t212 *ref_int32_x = ref__Ref_5int32(value__207)
    return t212
}

func println__T_int32(value__1 int32) struct{} {
    var t214 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t214)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter160 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Numbers_i_into__iter(source__4)
    Loop_loop220:
    for {
        var for_next161 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter160)
        switch for_next161.(type) {
        case None:
            break Loop_loop220
        case Some:
            var x162 int32 = for_next161.(Some)._0
            var t222 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
            var t223 int32 = t222 + x162
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t223)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t219 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    return t219
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter160 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(source__4)
    Loop_loop228:
    for {
        var for_next161 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter160)
        switch for_next161.(type) {
        case None:
            break Loop_loop228
        case Some:
            var x162 int32 = for_next161.(Some)._0
            var t230 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
            var t231 int32 = t230 + x162
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t231)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t227 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    return t227
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter160 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(source__4)
    Loop_loop236:
    for {
        var for_next161 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter160)
        switch for_next161.(type) {
        case None:
            break Loop_loop236
        case Some:
            var x162 int32 = for_next161.(Some)._0
            var t238 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
            var t239 int32 = t238 + x162
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t239)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t235 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    return t235
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var t242 []int32 = self__175.items[start__176:end__177]
    return t242
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var t245 int = vec_len__Vec_5int32(self__137)
    return t245
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var t248 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    return t248
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t251 string = _goml_runtime_core_int32_to_string(self__43)
    return t251
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var t254 func() Option__int32 = self__102.next_fn
    var t255 Option__int32 = t254()
    return t255
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__183 *_goml_vec_int32) FnIterator__int32 {
    var t258 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__183)
    return t258
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__195 []int32) FnIterator__int32 {
    var t261 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__195)
    return t261
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__190 []int32) FnIterator__int32 {
    var index__191 *ref_int_x = ref__Ref_3int(0)
    var len__192 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__190)
    var t264 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__191,
        len_1: len__192,
        self_2: self__190,
    }
    var t265 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t264)
    })
    return t265
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__186 []int32) int {
    var t268 int = len(self__186)
    return t268
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__184 []int32, index__185 int) int32 {
    var t271 int32 = self__184[index__185]
    return t271
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env173 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__179 *ref_int_x = env173.index_0
    var len__180 int = env173.len_1
    var self__178 *_goml_vec_int32 = env173.self_2
    var current__181 int = ref_get__Ref_3int(index__179)
    var t288 bool = current__181 < len__180
    if t288 {
        var value__182 int32 = vec_get__Vec_5int32(self__178, current__181)
        var t289 int = current__181 + 1
        ref_set__Ref_3int(index__179, t289)
        var t290 Option__int32 = Some{
            _0: value__182,
        }
        return t290
    } else {
        return None{}
    }
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env174 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var index__191 *ref_int_x = env174.index_0
    var len__192 int = env174.len_1
    var self__190 []int32 = env174.self_2
    var current__193 int = ref_get__Ref_3int(index__191)
    var t295 bool = current__193 < len__192
    if t295 {
        var value__194 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__190, current__193)
        var t296 int = current__193 + 1
        ref_set__Ref_3int(index__191, t296)
        var t297 Option__int32 = Some{
            _0: value__194,
        }
        return t297
    } else {
        return None{}
    }
}

func main() {
    main0()
}
