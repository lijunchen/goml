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
    var inline197 string = "if"
    var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline197)
    _goml_runtime_core_string_println(inline198)
    var mtmp137 int = 1
    switch mtmp137 {
    case 1:
        var inline201 string = "match"
        var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline201)
        _goml_runtime_core_string_println(inline202)
    default:
    }
    var index__0 *ref_int_x
    var inline230 int = 0
    var inline231 *ref_int_x = ref__Ref_3int(inline230)
    index__0 = inline231
    Loop_loop158:
    for {
        var t159 int
        var inline216 int = ref_get__Ref_3int(index__0)
        t159 = inline216
        var t160 bool = t159 < 2
        if t160 {
            var t161 int
            var inline214 int = ref_get__Ref_3int(index__0)
            t161 = inline214
            var t162 string
            var inline212 string = _goml_runtime_core_int_to_string(t161)
            t162 = inline212
            var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
            _goml_runtime_core_string_println(inline209)
            var t163 int
            var inline207 int = ref_get__Ref_3int(index__0)
            t163 = inline207
            var t164 int = t163 + 1
            ref_set__Ref_3int(index__0, t164)
            continue
        } else {
            break Loop_loop158
        }
    }
    var values__1 *_goml_vec_string
    var inline228 *_goml_vec_string = vec_new__Vec_6string()
    values__1 = inline228
    var inline225 string = "for"
    vec_push__Vec_6string(values__1, inline225)
    var for_limit143 int = vec_len__Vec_6string(values__1)
    var for_index144 int = 0
    Loop_loop154:
    for {
        var t155 bool = for_index144 < for_limit143
        if t155 {
            var for_item145 string = vec_get__Vec_6string(values__1, for_index144)
            var t156 int = for_index144 + 1
            for_index144 = t156
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item145)
            _goml_runtime_core_string_println(inline218)
            continue
        } else {
            break Loop_loop154
        }
    }
    var inline221 string = "done"
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline221)
    _goml_runtime_core_string_println(inline222)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
