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

func vec_reserve__Vec_5int32(vec *_goml_vec_int32, additional int) struct{} {
    vec.items = _goml_slices.Grow(vec.items, int(additional))
    return struct{}{}
}

func print_values(values__0 *_goml_vec_int32) struct{} {
    var t189 int
    var inline233 int = vec_len__Vec_5int32(values__0)
    t189 = inline233
    var inline230 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t189)
    _goml_runtime_core_string_println(inline230)
    var for_limit174 int = vec_len__Vec_5int32(values__0)
    var for_index175 int = 0
    Loop_loop191:
    for {
        var t192 bool = for_index175 < for_limit174
        if t192 {
            var for_item176 int32 = vec_get__Vec_5int32(values__0, for_index175)
            var t193 int = for_index175 + 1
            for_index175 = t193
            var inline227 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(for_item176)
            _goml_runtime_core_string_println(inline227)
            continue
        } else {
            break Loop_loop191
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var values__2 *_goml_vec_int32
    var inline254 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__2 = inline254
    var inline251 int32 = 1
    vec_push__Vec_5int32(values__2, inline251)
    var inline248 int32 = 2
    vec_push__Vec_5int32(values__2, inline248)
    var inline245 int32 = 3
    vec_push__Vec_5int32(values__2, inline245)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(values__2, values__2)
    print_values(values__2)
    var aliased__3 *_goml_vec_int32
    var inline243 *_goml_vec_int32 = vec_new__Vec_5int32()
    aliased__3 = inline243
    var inline240 int32 = 4
    vec_push__Vec_5int32(aliased__3, inline240)
    var inline237 int32 = 5
    vec_push__Vec_5int32(aliased__3, inline237)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(aliased__3, aliased__3)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32
    var inline235 *_goml_vec_int32 = vec_new__Vec_5int32()
    empty__5 = inline235
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__219 *_goml_vec_int32, other__220 *_goml_vec_int32) struct{} {
    var len__221 int
    var inline264 int = vec_len__Vec_5int32(other__220)
    len__221 = inline264
    vec_reserve__Vec_5int32(self__219, len__221)
    var index__222 int = 0
    Loop_loop213:
    for {
        var t214 bool = index__222 < len__221
        if t214 {
            var t215 int32 = vec_get__Vec_5int32(other__220, index__222)
            vec_push__Vec_5int32(self__219, t215)
            var compound_old138 int = index__222
            var compound_value139 int = 1
            var t216 int = compound_old138 + compound_value139
            index__222 = t216
            continue
        } else {
            break Loop_loop213
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t220 string = _goml_runtime_core_int_to_string(self__67)
    return t220
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t223 string = _goml_runtime_core_int32_to_string(self__70)
    return t223
}

func main() {
    main0()
}
