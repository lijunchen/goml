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
    var t194 string = to_text__0(7)
    var inline282 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline282)
    var get__1 func(*_goml_vec_int32, int) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32
    var inline280 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline280
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t195 int32 = get__1(values__3, 1)
    var inline277 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t195)
    _goml_runtime_core_string_println(inline277)
    var make_slice__4 func(*_goml_vec_int32, int, int) []int32 = func(p0 *_goml_vec_int32, p1 int, p2 int) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int) int32 = func(p0 []int32, p1 int) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t196 int32 = slice_get_value__5(view__6, 0)
    var inline274 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t196)
    _goml_runtime_core_string_println(inline274)
    var inline271 int32 = 33
    vec_push__Vec_5int32(values__3, inline271)
    var t197 int
    var inline269 int = vec_len__Vec_5int32(values__3)
    t197 = inline269
    var inline266 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t197)
    _goml_runtime_core_string_println(inline266)
    var inline262 int = 0
    var inline263 int32 = 44
    vec_set__Vec_5int32(values__3, inline262, inline263)
    var t198 int32
    var inline259 int = 0
    var inline260 int32 = vec_get__Vec_5int32(values__3, inline259)
    t198 = inline260
    var inline256 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
    _goml_runtime_core_string_println(inline256)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t199 int
    var inline254 int = vec_len__Vec_5int32(values__3)
    t199 = inline254
    var inline251 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t199)
    _goml_runtime_core_string_println(inline251)
    var t200 int
    var inline249 int = vec_len__Vec_5int32(copied__8)
    t200 = inline249
    var inline246 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t200)
    _goml_runtime_core_string_println(inline246)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__176 *_goml_vec_int32, elem__177 int32) *_goml_vec_int32 {
    var t225 int
    var inline298 int = vec_len__Vec_5int32(self__176)
    t225 = inline298
    var t226 int = t225 + 1
    var result__178 *_goml_vec_int32
    var inline296 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t226)
    result__178 = inline296
    var index__179 int = 0
    Loop_loop228:
    for {
        var t229 int
        var inline292 int = vec_len__Vec_5int32(self__176)
        t229 = inline292
        var t230 bool = index__179 < t229
        if t230 {
            var t231 int32 = vec_get__Vec_5int32(self__176, index__179)
            vec_push__Vec_5int32(result__178, t231)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t232 int = compound_old80 + compound_value81
            index__179 = t232
            continue
        } else {
            break Loop_loop228
        }
    }
    vec_push__Vec_5int32(result__178, elem__177)
    return result__178
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t238 string = _goml_runtime_core_int32_to_string(self__70)
    return t238
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t241 string = _goml_runtime_core_int_to_string(self__67)
    return t241
}

func main() {
    main0()
}
