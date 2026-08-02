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
    _goml_runtime_core_string_println("if")
    var mtmp156 int = 1
    switch mtmp156 {
    case 1:
        _goml_runtime_core_string_println("match")
    default:
    }
    var index__0 *ref_int_x
    var inline226 int = 0
    var inline227 *ref_int_x = ref__Ref_3int(inline226)
    index__0 = inline227
    Loop_loop177:
    for {
        var t178 int
        var inline219 int = ref_get__Ref_3int(index__0)
        t178 = inline219
        var t179 bool = t178 < 2
        if t179 {
            var t180 int
            var inline217 int = ref_get__Ref_3int(index__0)
            t180 = inline217
            var t181 string
            var inline215 string = _goml_runtime_core_int_to_string(t180)
            t181 = inline215
            _goml_runtime_core_string_println(t181)
            var t182 int
            var inline213 int = ref_get__Ref_3int(index__0)
            t182 = inline213
            var t183 int = t182 + 1
            ref_set__Ref_3int(index__0, t183)
            continue
        } else {
            break Loop_loop177
        }
    }
    var values__1 *_goml_vec_string
    var inline224 *_goml_vec_string = vec_new__Vec_6string()
    values__1 = inline224
    var inline221 string = "for"
    vec_push__Vec_6string(values__1, inline221)
    var for_limit162 int = vec_len__Vec_6string(values__1)
    var for_index163 int = 0
    Loop_loop173:
    for {
        var t174 bool = for_index163 < for_limit162
        if t174 {
            var for_item164 string = vec_get__Vec_6string(values__1, for_index163)
            var t175 int = for_index163 + 1
            for_index163 = t175
            _goml_runtime_core_string_println(for_item164)
            continue
        } else {
            break Loop_loop173
        }
    }
    _goml_runtime_core_string_println("done")
    return struct{}{}
}

func main() {
    main0()
}
