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
    var mtmp109 int = 1
    switch mtmp109 {
    case 1:
        _goml_runtime_core_string_println("match")
    default:
    }
    var index__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop130:
    for {
        var t131 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
        var t132 bool = t131 < 2
        if t132 {
            var t133 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
            var t134 string = _goml_m_inherent_i_int_i_int_i_to__string(t133)
            _goml_runtime_core_string_println(t134)
            var t135 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
            var t136 int = t135 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__0, t136)
            continue
        } else {
            break Loop_loop130
        }
    }
    var values__1 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__1, "for")
    var for_source114 *_goml_vec_string = values__1
    var for_limit115 int = vec_len__Vec_6string(for_source114)
    var for_index116 int = 0
    Loop_loop126:
    for {
        var t127 bool = for_index116 < for_limit115
        if t127 {
            var for_item117 string = vec_get__Vec_6string(for_source114, for_index116)
            var t128 int = for_index116 + 1
            for_index116 = t128
            var value__2 string = for_item117
            _goml_runtime_core_string_println(value__2)
            continue
        } else {
            break Loop_loop126
        }
    }
    _goml_runtime_core_string_println("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv142 *ref_int_x
    var t143 *ref_int_x = ref__Ref_3int(value__207)
    retv142 = t143
    return retv142
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv145 int
    var t146 int = ref_get__Ref_3int(self__208)
    retv145 = t146
    return retv145
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv148 string
    var t149 string = _goml_runtime_core_int_to_string(self__5)
    retv148 = t149
    return retv148
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv153 *_goml_vec_string
    var t154 *_goml_vec_string = vec_new__Vec_6string()
    retv153 = t154
    return retv153
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func main() {
    main0()
}
