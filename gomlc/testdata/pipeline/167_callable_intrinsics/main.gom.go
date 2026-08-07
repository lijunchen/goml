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
    var t148 string = to_text__0(7)
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
    _goml_runtime_core_string_println(inline236)
    var get__1 func(*_goml_vec_int32, int) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32
    var inline234 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline234
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t149 int32 = get__1(values__3, 1)
    var inline231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t149)
    _goml_runtime_core_string_println(inline231)
    var make_slice__4 func(*_goml_vec_int32, int, int) []int32 = func(p0 *_goml_vec_int32, p1 int, p2 int) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int) int32 = func(p0 []int32, p1 int) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t150 int32 = slice_get_value__5(view__6, 0)
    var inline228 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t150)
    _goml_runtime_core_string_println(inline228)
    var inline225 int32 = 33
    vec_push__Vec_5int32(values__3, inline225)
    var t151 int
    var inline223 int = vec_len__Vec_5int32(values__3)
    t151 = inline223
    var inline220 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t151)
    _goml_runtime_core_string_println(inline220)
    var inline216 int = 0
    var inline217 int32 = 44
    vec_set__Vec_5int32(values__3, inline216, inline217)
    var t152 int32
    var inline213 int = 0
    var inline214 int32 = vec_get__Vec_5int32(values__3, inline213)
    t152 = inline214
    var inline210 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t152)
    _goml_runtime_core_string_println(inline210)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t153 int
    var inline208 int = vec_len__Vec_5int32(values__3)
    t153 = inline208
    var inline205 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t153)
    _goml_runtime_core_string_println(inline205)
    var t154 int
    var inline203 int = vec_len__Vec_5int32(copied__8)
    t154 = inline203
    var inline200 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t154)
    _goml_runtime_core_string_println(inline200)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__153 *_goml_vec_int32, elem__154 int32) *_goml_vec_int32 {
    var t179 int
    var inline252 int = vec_len__Vec_5int32(self__153)
    t179 = inline252
    var t180 int = t179 + 1
    var result__155 *_goml_vec_int32
    var inline250 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t180)
    result__155 = inline250
    var index__156 int = 0
    Loop_loop182:
    for {
        var t183 int
        var inline246 int = vec_len__Vec_5int32(self__153)
        t183 = inline246
        var t184 bool = index__156 < t183
        if t184 {
            var t185 int32 = vec_get__Vec_5int32(self__153, index__156)
            vec_push__Vec_5int32(result__155, t185)
            var compound_old44 int = index__156
            var compound_value45 int = 1
            var t186 int = compound_old44 + compound_value45
            index__156 = t186
            continue
        } else {
            break Loop_loop182
        }
    }
    vec_push__Vec_5int32(result__155, elem__154)
    return result__155
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t192 string = _goml_runtime_core_int32_to_string(self__72)
    return t192
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t195 string = _goml_runtime_core_int_to_string(self__69)
    return t195
}

func main() {
    main0()
}
