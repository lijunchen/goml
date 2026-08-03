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
    var inline238 string = "if"
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline238)
    _goml_runtime_core_string_println(inline239)
    var mtmp178 int = 1
    switch mtmp178 {
    case 1:
        var inline242 string = "match"
        var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline242)
        _goml_runtime_core_string_println(inline243)
    default:
    }
    var index__0 *ref_int_x
    var inline271 int = 0
    var inline272 *ref_int_x = ref__Ref_3int(inline271)
    index__0 = inline272
    Loop_loop199:
    for {
        var t200 int
        var inline257 int = ref_get__Ref_3int(index__0)
        t200 = inline257
        var t201 bool = t200 < 2
        if t201 {
            var t202 int
            var inline255 int = ref_get__Ref_3int(index__0)
            t202 = inline255
            var t203 string
            var inline253 string = _goml_runtime_core_int_to_string(t202)
            t203 = inline253
            var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
            _goml_runtime_core_string_println(inline250)
            var t204 int
            var inline248 int = ref_get__Ref_3int(index__0)
            t204 = inline248
            var t205 int = t204 + 1
            ref_set__Ref_3int(index__0, t205)
            continue
        } else {
            break Loop_loop199
        }
    }
    var values__1 *_goml_vec_string
    var inline269 *_goml_vec_string = vec_new__Vec_6string()
    values__1 = inline269
    var inline266 string = "for"
    vec_push__Vec_6string(values__1, inline266)
    var for_limit184 int = vec_len__Vec_6string(values__1)
    var for_index185 int = 0
    Loop_loop195:
    for {
        var t196 bool = for_index185 < for_limit184
        if t196 {
            var for_item186 string = vec_get__Vec_6string(values__1, for_index185)
            var t197 int = for_index185 + 1
            for_index185 = t197
            var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item186)
            _goml_runtime_core_string_println(inline259)
            continue
        } else {
            break Loop_loop195
        }
    }
    var inline262 string = "done"
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline262)
    _goml_runtime_core_string_println(inline263)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
