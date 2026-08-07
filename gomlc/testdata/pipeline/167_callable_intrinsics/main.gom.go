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
    var t184 string = to_text__0(7)
    var inline272 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline272)
    var get__1 func(*_goml_vec_int32, int) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32
    var inline270 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline270
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t185 int32 = get__1(values__3, 1)
    var inline267 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t185)
    _goml_runtime_core_string_println(inline267)
    var make_slice__4 func(*_goml_vec_int32, int, int) []int32 = func(p0 *_goml_vec_int32, p1 int, p2 int) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int) int32 = func(p0 []int32, p1 int) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t186 int32 = slice_get_value__5(view__6, 0)
    var inline264 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t186)
    _goml_runtime_core_string_println(inline264)
    var inline261 int32 = 33
    vec_push__Vec_5int32(values__3, inline261)
    var t187 int
    var inline259 int = vec_len__Vec_5int32(values__3)
    t187 = inline259
    var inline256 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t187)
    _goml_runtime_core_string_println(inline256)
    var inline252 int = 0
    var inline253 int32 = 44
    vec_set__Vec_5int32(values__3, inline252, inline253)
    var t188 int32
    var inline249 int = 0
    var inline250 int32 = vec_get__Vec_5int32(values__3, inline249)
    t188 = inline250
    var inline246 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t188)
    _goml_runtime_core_string_println(inline246)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(values__3, 55)
    var t189 int
    var inline244 int = vec_len__Vec_5int32(values__3)
    t189 = inline244
    var inline241 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t189)
    _goml_runtime_core_string_println(inline241)
    var t190 int
    var inline239 int = vec_len__Vec_5int32(copied__8)
    t190 = inline239
    var inline236 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t190)
    _goml_runtime_core_string_println(inline236)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__178 *_goml_vec_int32, elem__179 int32) *_goml_vec_int32 {
    var t215 int
    var inline288 int = vec_len__Vec_5int32(self__178)
    t215 = inline288
    var t216 int = t215 + 1
    var result__180 *_goml_vec_int32
    var inline286 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t216)
    result__180 = inline286
    var index__181 int = 0
    Loop_loop218:
    for {
        var t219 int
        var inline282 int = vec_len__Vec_5int32(self__178)
        t219 = inline282
        var t220 bool = index__181 < t219
        if t220 {
            var t221 int32 = vec_get__Vec_5int32(self__178, index__181)
            vec_push__Vec_5int32(result__180, t221)
            var compound_old80 int = index__181
            var compound_value81 int = 1
            var t222 int = compound_old80 + compound_value81
            index__181 = t222
            continue
        } else {
            break Loop_loop218
        }
    }
    vec_push__Vec_5int32(result__180, elem__179)
    return result__180
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t228 string = _goml_runtime_core_int32_to_string(self__72)
    return t228
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t231 string = _goml_runtime_core_int_to_string(self__69)
    return t231
}

func main() {
    main0()
}
