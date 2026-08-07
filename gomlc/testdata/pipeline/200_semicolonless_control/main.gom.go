package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
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

func main0() struct{} {
    var inline233 string = "if"
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline233)
    _goml_runtime_core_string_println(inline234)
    var mtmp173 int = 1
    switch mtmp173 {
    case 1:
        var inline237 string = "match"
        var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline237)
        _goml_runtime_core_string_println(inline238)
    default:
    }
    var index__0 *ref_int_x
    var inline266 int = 0
    var inline267 *ref_int_x = ref__Ref_3int(inline266)
    index__0 = inline267
    Loop_loop194:
    for {
        var t195 int
        var inline252 int = ref_get__Ref_3int(index__0)
        t195 = inline252
        var t196 bool = t195 < 2
        if t196 {
            var t197 int
            var inline250 int = ref_get__Ref_3int(index__0)
            t197 = inline250
            var t198 string
            var inline248 string = _goml_runtime_core_int_to_string(t197)
            t198 = inline248
            var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline245)
            var t199 int
            var inline243 int = ref_get__Ref_3int(index__0)
            t199 = inline243
            var t200 int = t199 + 1
            ref_set__Ref_3int(index__0, t200)
            continue
        } else {
            break Loop_loop194
        }
    }
    var values__1 *_goml_vec_string
    var inline264 *_goml_vec_string = vec_new__Vec_6string()
    values__1 = inline264
    var inline261 string = "for"
    vec_push__Vec_6string(values__1, inline261)
    var for_limit179 int = vec_len__Vec_6string(values__1)
    var for_index180 int = 0
    Loop_loop190:
    for {
        var t191 bool = for_index180 < for_limit179
        if t191 {
            var for_item181 string = vec_get__Vec_6string(values__1, for_index180)
            var t192 int = for_index180 + 1
            for_index180 = t192
            var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item181)
            _goml_runtime_core_string_println(inline254)
            continue
        } else {
            break Loop_loop190
        }
    }
    var inline257 string = "done"
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline257)
    _goml_runtime_core_string_println(inline258)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
