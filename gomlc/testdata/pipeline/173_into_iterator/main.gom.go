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
    var retv129 FnIterator__int32
    var t130 *ref_int32_x = self__0.conversions
    var t131 *ref_int32_x = self__0.conversions
    var t132 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t131)
    var t133 int32 = t132 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t130, t133)
    var t134 *_goml_vec_int32 = self__0.values
    var t135 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t134)
    retv129 = t135
    return retv129
}

func make_numbers(builds__1 *ref_int32_x, conversions__2 *ref_int32_x) Numbers {
    var retv137 Numbers
    var t138 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__1)
    var t139 int32 = t138 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__1, t139)
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 3)
    var t140 Numbers = Numbers{
        values: values__3,
        conversions: conversions__2,
    }
    retv137 = t140
    return retv137
}

func main0() struct{} {
    var builds__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var conversions__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t142 Numbers = make_numbers(builds__7, conversions__8)
    var t143 int32 = sum__S_Numbers(t142)
    println__T_int32(t143)
    var t144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    println__T_int32(t144)
    var t145 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(conversions__8)
    println__T_int32(t145)
    var values__9 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 30)
    var t146 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    println__T_int32(t146)
    var t147 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__9, 1, 3)
    var t148 int32 = _goml_m_sum____S__Slice_l_int32_r_(t147)
    println__T_int32(t148)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv150 int32
    var t151 int32 = ref_get__Ref_5int32(self__208)
    retv150 = t151
    return retv150
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__178 *_goml_vec_int32) FnIterator__int32 {
    var retv155 FnIterator__int32
    var index__179 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__180 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__178)
    var t156 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__179,
        len_1: len__180,
        self_2: self__178,
    }
    var t157 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t156)
    })
    retv155 = t157
    return retv155
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv159 *_goml_vec_int32
    var t160 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv159 = t160
    return retv159
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv164 *ref_int32_x
    var t165 *ref_int32_x = ref__Ref_5int32(value__207)
    retv164 = t165
    return retv164
}

func println__T_int32(value__1 int32) struct{} {
    var t167 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t167)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var retv170 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter113 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Numbers_i_into__iter(source__4)
    Loop_loop173:
    for {
        if true {
            var for_next114 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter113)
            switch for_next114.(type) {
            case None:
                break Loop_loop173
            case Some:
                var x115 int32 = for_next114.(Some)._0
                var value__6 int32 = x115
                var t175 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t176 int32 = t175 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t176)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop173
        }
    }
    var t172 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv170 = t172
    return retv170
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var retv178 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter113 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(source__4)
    Loop_loop181:
    for {
        if true {
            var for_next114 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter113)
            switch for_next114.(type) {
            case None:
                break Loop_loop181
            case Some:
                var x115 int32 = for_next114.(Some)._0
                var value__6 int32 = x115
                var t183 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t184 int32 = t183 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t184)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop181
        }
    }
    var t180 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv178 = t180
    return retv178
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var retv186 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter113 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(source__4)
    Loop_loop189:
    for {
        if true {
            var for_next114 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter113)
            switch for_next114.(type) {
            case None:
                break Loop_loop189
            case Some:
                var x115 int32 = for_next114.(Some)._0
                var value__6 int32 = x115
                var t191 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t192 int32 = t191 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t192)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop189
        }
    }
    var t188 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv186 = t188
    return retv186
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var retv194 []int32
    var t195 []int32 = self__175.items[start__176:end__177]
    retv194 = t195
    return retv194
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv197 *ref_int_x
    var t198 *ref_int_x = ref__Ref_3int(value__207)
    retv197 = t198
    return retv197
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv200 int
    var t201 int = vec_len__Vec_5int32(self__137)
    retv200 = t201
    return retv200
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv203 int
    var t204 int = ref_get__Ref_3int(self__208)
    retv203 = t204
    return retv203
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv206 int32
    var t207 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv206 = t207
    return retv206
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__101 func() Option__int32) FnIterator__int32 {
    var retv211 FnIterator__int32
    var t212 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__101,
    }
    retv211 = t212
    return retv211
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv214 string
    var t215 string = _goml_runtime_core_int32_to_string(self__43)
    retv214 = t215
    return retv214
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__102 FnIterator__int32) Option__int32 {
    var retv217 Option__int32
    var t218 func() Option__int32 = self__102.next_fn
    var t219 Option__int32 = t218()
    retv217 = t219
    return retv217
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__183 *_goml_vec_int32) FnIterator__int32 {
    var retv221 FnIterator__int32
    var t222 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__183)
    retv221 = t222
    return retv221
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__195 []int32) FnIterator__int32 {
    var retv224 FnIterator__int32
    var t225 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__195)
    retv224 = t225
    return retv224
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__190 []int32) FnIterator__int32 {
    var retv227 FnIterator__int32
    var index__191 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__192 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__190)
    var t228 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__191,
        len_1: len__192,
        self_2: self__190,
    }
    var t229 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t228)
    })
    retv227 = t229
    return retv227
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__186 []int32) int {
    var retv231 int
    var t232 int = len(self__186)
    retv231 = t232
    return retv231
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__184 []int32, index__185 int) int32 {
    var retv234 int32
    var t235 int32 = self__184[index__185]
    retv234 = t235
    return retv234
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env126 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv249 Option__int32
    var index__179 *ref_int_x = env126.index_0
    var len__180 int = env126.len_1
    var self__178 *_goml_vec_int32 = env126.self_2
    var current__181 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__179)
    var t252 bool = current__181 < len__180
    var jp251 Option__int32
    if t252 {
        var value__182 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__178, current__181)
        var t253 int = current__181 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__179, t253)
        var t254 Option__int32 = Some{
            _0: value__182,
        }
        jp251 = t254
    } else {
        jp251 = None{}
    }
    retv249 = jp251
    return retv249
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env127 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var retv256 Option__int32
    var index__191 *ref_int_x = env127.index_0
    var len__192 int = env127.len_1
    var self__190 []int32 = env127.self_2
    var current__193 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__191)
    var t259 bool = current__193 < len__192
    var jp258 Option__int32
    if t259 {
        var value__194 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__190, current__193)
        var t260 int = current__193 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__191, t260)
        var t261 Option__int32 = Some{
            _0: value__194,
        }
        jp258 = t261
    } else {
        jp258 = None{}
    }
    retv256 = jp258
    return retv256
}

func main() {
    main0()
}
