package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
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

func vec_with_capacity__Vec_5int32(capacity int) *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: _goml_slices.Grow([]int32{}, int(capacity)),
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

func main0() struct{} {
    var to_text__0 func(int32) string = _goml_runtime_core_int32_to_string
    var t189 string = to_text__0(7)
    var inline277 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline277)
    var get__1 func(*_goml_vec_int32, int) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32
    var inline275 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline275
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t190 int32 = get__1(values__3, 1)
    var inline272 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t190)
    _goml_runtime_core_string_println(inline272)
    var make_slice__4 func(*_goml_vec_int32, int, int) []int32 = func(p0 *_goml_vec_int32, p1 int, p2 int) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int) int32 = func(p0 []int32, p1 int) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t191 int32 = slice_get_value__5(view__6, 0)
    var inline269 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t191)
    _goml_runtime_core_string_println(inline269)
    var inline266 int32 = 33
    vec_push__Vec_5int32(values__3, inline266)
    var t192 int
    var inline264 int = vec_len__Vec_5int32(values__3)
    t192 = inline264
    var inline261 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t192)
    _goml_runtime_core_string_println(inline261)
    var inline257 int = 0
    var inline258 int32 = 44
    vec_set__Vec_5int32(values__3, inline257, inline258)
    var t193 int32
    var inline254 int = 0
    var inline255 int32 = vec_get__Vec_5int32(values__3, inline254)
    t193 = inline255
    var inline251 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t193)
    _goml_runtime_core_string_println(inline251)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t194 int
    var inline249 int = vec_len__Vec_5int32(values__3)
    t194 = inline249
    var inline246 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t194)
    _goml_runtime_core_string_println(inline246)
    var t195 int
    var inline244 int = vec_len__Vec_5int32(copied__8)
    t195 = inline244
    var inline241 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t195)
    _goml_runtime_core_string_println(inline241)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__157 *_goml_vec_int32, elem__158 int32) *_goml_vec_int32 {
    var t220 int
    var inline293 int = vec_len__Vec_5int32(self__157)
    t220 = inline293
    var t221 int = t220 + 1
    var result__159 *_goml_vec_int32
    var inline291 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t221)
    result__159 = inline291
    var index__160 int = 0
    Loop_loop223:
    for {
        var t224 int
        var inline287 int = vec_len__Vec_5int32(self__157)
        t224 = inline287
        var t225 bool = index__160 < t224
        if t225 {
            var t226 int32 = vec_get__Vec_5int32(self__157, index__160)
            vec_push__Vec_5int32(result__159, t226)
            var compound_old60 int = index__160
            var compound_value61 int = 1
            var t227 int = compound_old60 + compound_value61
            index__160 = t227
            continue
        } else {
            break Loop_loop223
        }
    }
    vec_push__Vec_5int32(result__159, elem__158)
    return result__159
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t233 string = _goml_runtime_core_int32_to_string(self__72)
    return t233
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t236 string = _goml_runtime_core_int_to_string(self__69)
    return t236
}

func main() {
    main0()
}
