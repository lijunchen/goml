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
    var inline243 string = "if"
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline243)
    _goml_runtime_core_string_println(inline244)
    var mtmp183 int = 1
    switch mtmp183 {
    case 1:
        var inline247 string = "match"
        var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline247)
        _goml_runtime_core_string_println(inline248)
    default:
    }
    var index__0 *ref_int_x
    var inline276 int = 0
    var inline277 *ref_int_x = ref__Ref_3int(inline276)
    index__0 = inline277
    Loop_loop204:
    for {
        var t205 int
        var inline262 int = ref_get__Ref_3int(index__0)
        t205 = inline262
        var t206 bool = t205 < 2
        if t206 {
            var t207 int
            var inline260 int = ref_get__Ref_3int(index__0)
            t207 = inline260
            var t208 string
            var inline258 string = _goml_runtime_core_int_to_string(t207)
            t208 = inline258
            var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
            _goml_runtime_core_string_println(inline255)
            var t209 int
            var inline253 int = ref_get__Ref_3int(index__0)
            t209 = inline253
            var t210 int = t209 + 1
            ref_set__Ref_3int(index__0, t210)
            continue
        } else {
            break Loop_loop204
        }
    }
    var values__1 *_goml_vec_string
    var inline274 *_goml_vec_string = vec_new__Vec_6string()
    values__1 = inline274
    var inline271 string = "for"
    vec_push__Vec_6string(values__1, inline271)
    var for_limit189 int = vec_len__Vec_6string(values__1)
    var for_index190 int = 0
    Loop_loop200:
    for {
        var t201 bool = for_index190 < for_limit189
        if t201 {
            var for_item191 string = vec_get__Vec_6string(values__1, for_index190)
            var t202 int = for_index190 + 1
            for_index190 = t202
            var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item191)
            _goml_runtime_core_string_println(inline264)
            continue
        } else {
            break Loop_loop200
        }
    }
    var inline267 string = "done"
    var inline268 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline267)
    _goml_runtime_core_string_println(inline268)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
