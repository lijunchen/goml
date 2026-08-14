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
    var t199 int
    var inline243 int = vec_len__Vec_5int32(values__0)
    t199 = inline243
    var inline240 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t199)
    _goml_runtime_core_string_println(inline240)
    var for_limit184 int = vec_len__Vec_5int32(values__0)
    var for_index185 int = 0
    Loop_loop201:
    for {
        var t202 bool = for_index185 < for_limit184
        if t202 {
            var for_item186 int32 = vec_get__Vec_5int32(values__0, for_index185)
            var t203 int = for_index185 + 1
            for_index185 = t203
            var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(for_item186)
            _goml_runtime_core_string_println(inline237)
            continue
        } else {
            break Loop_loop201
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var values__2 *_goml_vec_int32
    var inline264 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__2 = inline264
    var inline261 int32 = 1
    vec_push__Vec_5int32(values__2, inline261)
    var inline258 int32 = 2
    vec_push__Vec_5int32(values__2, inline258)
    var inline255 int32 = 3
    vec_push__Vec_5int32(values__2, inline255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(values__2, values__2)
    print_values(values__2)
    var aliased__3 *_goml_vec_int32
    var inline253 *_goml_vec_int32 = vec_new__Vec_5int32()
    aliased__3 = inline253
    var inline250 int32 = 4
    vec_push__Vec_5int32(aliased__3, inline250)
    var inline247 int32 = 5
    vec_push__Vec_5int32(aliased__3, inline247)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(aliased__3, aliased__3)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32
    var inline245 *_goml_vec_int32 = vec_new__Vec_5int32()
    empty__5 = inline245
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__223 *_goml_vec_int32, other__224 *_goml_vec_int32) struct{} {
    var len__225 int
    var inline274 int = vec_len__Vec_5int32(other__224)
    len__225 = inline274
    vec_reserve__Vec_5int32(self__223, len__225)
    var index__226 int = 0
    Loop_loop223:
    for {
        var t224 bool = index__226 < len__225
        if t224 {
            var t225 int32 = vec_get__Vec_5int32(other__224, index__226)
            vec_push__Vec_5int32(self__223, t225)
            var compound_old143 int = index__226
            var compound_value144 int = 1
            var t226 int = compound_old143 + compound_value144
            index__226 = t226
            continue
        } else {
            break Loop_loop223
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t230 string = _goml_runtime_core_int_to_string(self__67)
    return t230
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t233 string = _goml_runtime_core_int32_to_string(self__70)
    return t233
}

func main() {
    main0()
}
