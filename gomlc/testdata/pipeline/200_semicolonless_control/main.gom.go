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
    if true {
        _goml_runtime_core_string_println("if")
    } else {
        _goml_runtime_core_string_println("else")
    }
    var mtmp156 int = 1
    switch mtmp156 {
    case 1:
        _goml_runtime_core_string_println("match")
    default:
    }
    var index__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop177:
    for {
        var t178 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
        var t179 bool = t178 < 2
        if t179 {
            var t180 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
            var t181 string = _goml_m_inherent_i_int_i_int_i_to__string(t180)
            _goml_runtime_core_string_println(t181)
            var t182 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
            var t183 int = t182 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__0, t183)
            continue
        } else {
            break Loop_loop177
        }
    }
    var values__1 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__1, "for")
    var for_source161 *_goml_vec_string = values__1
    var for_limit162 int = vec_len__Vec_6string(for_source161)
    var for_index163 int = 0
    Loop_loop173:
    for {
        var t174 bool = for_index163 < for_limit162
        if t174 {
            var for_item164 string = vec_get__Vec_6string(for_source161, for_index163)
            var t175 int = for_index163 + 1
            for_index163 = t175
            var value__2 string = for_item164
            _goml_runtime_core_string_println(value__2)
            continue
        } else {
            break Loop_loop173
        }
    }
    _goml_runtime_core_string_println("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv189 *ref_int_x
    var t190 *ref_int_x = ref__Ref_3int(value__207)
    retv189 = t190
    return retv189
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv192 int
    var t193 int = ref_get__Ref_3int(self__208)
    retv192 = t193
    return retv192
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv195 string
    var t196 string = _goml_runtime_core_int_to_string(self__5)
    retv195 = t196
    return retv195
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv200 *_goml_vec_string
    var t201 *_goml_vec_string = vec_new__Vec_6string()
    retv200 = t201
    return retv200
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func main() {
    main0()
}
