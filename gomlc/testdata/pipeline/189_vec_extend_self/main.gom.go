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
    var t204 int
    var inline248 int = vec_len__Vec_5int32(values__0)
    t204 = inline248
    var inline245 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t204)
    _goml_runtime_core_string_println(inline245)
    var for_limit189 int = vec_len__Vec_5int32(values__0)
    var for_index190 int = 0
    Loop_loop206:
    for {
        var t207 bool = for_index190 < for_limit189
        if t207 {
            var for_item191 int32 = vec_get__Vec_5int32(values__0, for_index190)
            var t208 int = for_index190 + 1
            for_index190 = t208
            var inline242 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(for_item191)
            _goml_runtime_core_string_println(inline242)
            continue
        } else {
            break Loop_loop206
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var values__2 *_goml_vec_int32
    var inline269 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__2 = inline269
    var inline266 int32 = 1
    vec_push__Vec_5int32(values__2, inline266)
    var inline263 int32 = 2
    vec_push__Vec_5int32(values__2, inline263)
    var inline260 int32 = 3
    vec_push__Vec_5int32(values__2, inline260)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(values__2, values__2)
    print_values(values__2)
    var aliased__3 *_goml_vec_int32
    var inline258 *_goml_vec_int32 = vec_new__Vec_5int32()
    aliased__3 = inline258
    var inline255 int32 = 4
    vec_push__Vec_5int32(aliased__3, inline255)
    var inline252 int32 = 5
    vec_push__Vec_5int32(aliased__3, inline252)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(aliased__3, aliased__3)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32
    var inline250 *_goml_vec_int32 = vec_new__Vec_5int32()
    empty__5 = inline250
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__int32(self__223 *_goml_vec_int32, other__224 *_goml_vec_int32) struct{} {
    var len__225 int
    var inline279 int = vec_len__Vec_5int32(other__224)
    len__225 = inline279
    vec_reserve__Vec_5int32(self__223, len__225)
    var index__226 int = 0
    Loop_loop228:
    for {
        var t229 bool = index__226 < len__225
        if t229 {
            var t230 int32 = vec_get__Vec_5int32(other__224, index__226)
            vec_push__Vec_5int32(self__223, t230)
            var compound_old143 int = index__226
            var compound_value144 int = 1
            var t231 int = compound_old143 + compound_value144
            index__226 = t231
            continue
        } else {
            break Loop_loop228
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t235 string = _goml_runtime_core_int_to_string(self__67)
    return t235
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t238 string = _goml_runtime_core_int32_to_string(self__70)
    return t238
}

func main() {
    main0()
}
