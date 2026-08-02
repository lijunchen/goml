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
    var t167 string = to_text__0(7)
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline255)
    var get__1 func(*_goml_vec_int32, int) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32
    var inline253 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline253
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t168 int32 = get__1(values__3, 1)
    var inline250 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t168)
    _goml_runtime_core_string_println(inline250)
    var make_slice__4 func(*_goml_vec_int32, int, int) []int32 = func(p0 *_goml_vec_int32, p1 int, p2 int) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int) int32 = func(p0 []int32, p1 int) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t169 int32 = slice_get_value__5(view__6, 0)
    var inline247 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t169)
    _goml_runtime_core_string_println(inline247)
    var inline244 int32 = 33
    vec_push__Vec_5int32(values__3, inline244)
    var t170 int
    var inline242 int = vec_len__Vec_5int32(values__3)
    t170 = inline242
    var inline239 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t170)
    _goml_runtime_core_string_println(inline239)
    var inline235 int = 0
    var inline236 int32 = 44
    vec_set__Vec_5int32(values__3, inline235, inline236)
    var t171 int32
    var inline232 int = 0
    var inline233 int32 = vec_get__Vec_5int32(values__3, inline232)
    t171 = inline233
    var inline229 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t171)
    _goml_runtime_core_string_println(inline229)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t172 int
    var inline227 int = vec_len__Vec_5int32(values__3)
    t172 = inline227
    var inline224 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t172)
    _goml_runtime_core_string_println(inline224)
    var t173 int
    var inline222 int = vec_len__Vec_5int32(copied__8)
    t173 = inline222
    var inline219 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t173)
    _goml_runtime_core_string_println(inline219)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__128 *_goml_vec_int32, elem__129 int32) *_goml_vec_int32 {
    var t198 int
    var inline271 int = vec_len__Vec_5int32(self__128)
    t198 = inline271
    var t199 int = t198 + 1
    var result__130 *_goml_vec_int32
    var inline269 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t199)
    result__130 = inline269
    var index__131 int = 0
    Loop_loop201:
    for {
        var t202 int
        var inline265 int = vec_len__Vec_5int32(self__128)
        t202 = inline265
        var t203 bool = index__131 < t202
        if t203 {
            var t204 int32 = vec_get__Vec_5int32(self__128, index__131)
            vec_push__Vec_5int32(result__130, t204)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t205 int = compound_old38 + compound_value39
            index__131 = t205
            continue
        } else {
            break Loop_loop201
        }
    }
    vec_push__Vec_5int32(result__130, elem__129)
    return result__130
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t211 string = _goml_runtime_core_int32_to_string(self__43)
    return t211
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t214 string = _goml_runtime_core_int_to_string(self__40)
    return t214
}

func main() {
    main0()
}
