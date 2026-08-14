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
    var inline248 string = "if"
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline248)
    _goml_runtime_core_string_println(inline249)
    var mtmp188 int = 1
    switch mtmp188 {
    case 1:
        var inline252 string = "match"
        var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline252)
        _goml_runtime_core_string_println(inline253)
    default:
    }
    var index__0 *ref_int_x
    var inline281 int = 0
    var inline282 *ref_int_x = ref__Ref_3int(inline281)
    index__0 = inline282
    Loop_loop209:
    for {
        var t210 int
        var inline267 int = ref_get__Ref_3int(index__0)
        t210 = inline267
        var t211 bool = t210 < 2
        if t211 {
            var t212 int
            var inline265 int = ref_get__Ref_3int(index__0)
            t212 = inline265
            var t213 string
            var inline263 string = _goml_runtime_core_int_to_string(t212)
            t213 = inline263
            var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
            _goml_runtime_core_string_println(inline260)
            var t214 int
            var inline258 int = ref_get__Ref_3int(index__0)
            t214 = inline258
            var t215 int = t214 + 1
            ref_set__Ref_3int(index__0, t215)
            continue
        } else {
            break Loop_loop209
        }
    }
    var values__1 *_goml_vec_string
    var inline279 *_goml_vec_string = vec_new__Vec_6string()
    values__1 = inline279
    var inline276 string = "for"
    vec_push__Vec_6string(values__1, inline276)
    var for_limit194 int = vec_len__Vec_6string(values__1)
    var for_index195 int = 0
    Loop_loop205:
    for {
        var t206 bool = for_index195 < for_limit194
        if t206 {
            var for_item196 string = vec_get__Vec_6string(values__1, for_index195)
            var t207 int = for_index195 + 1
            for_index195 = t207
            var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item196)
            _goml_runtime_core_string_println(inline269)
            continue
        } else {
            break Loop_loop205
        }
    }
    var inline272 string = "done"
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline272)
    _goml_runtime_core_string_println(inline273)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
