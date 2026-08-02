package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
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

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func unwrap_or_negative(value__0 Option__int) int {
    var retv178 int
    var mtmp155 Option__int = value__0
    var jp180 int
    switch mtmp155.(type) {
    case Some:
        var x156 int = mtmp155.(Some)._0
        var result__1 int = x156
        jp180 = result__1
        retv178 = jp180
        return retv178
    default:
        retv178 = -1
        return retv178
    }
}

func count_to(limit__2 int) int {
    var retv182 int
    var counter__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var jp184 int
    Loop_loop_expr185:
    for {
        var current__4 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(counter__3)
        var t188 bool = current__4 >= limit__2
        if t188 {
            jp184 = current__4
            break Loop_loop_expr185
        } else {
            var t187 int = current__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(counter__3, t187)
            continue
        }
    }
    retv182 = jp184
    return retv182
}

func loop_option(value__5 Option__int) int {
    var retv190 int
    var jp192 int
    var mtmp161 Option__int = value__5
    switch mtmp161.(type) {
    case Some:
        var x162 int = mtmp161.(Some)._0
        var item__6 int = x162
        jp192 = item__6
        retv190 = jp192
        return retv190
    default:
        jp192 = -2
        retv190 = jp192
        return retv190
    }
}

func nested_loop_value() int {
    var retv196 int
    var jp198 int
    jp198 = 7
    retv196 = jp198
    return retv196
}

func matches_raw_path(value__7 string) bool {
    var retv208 bool
    var jp210 bool
    switch value__7 {
    case "C:\\tmp":
        jp210 = true
    default:
        jp210 = false
    }
    retv208 = jp210
    return retv208
}

func main0() struct{} {
    _goml_runtime_core_string_println("C:\\tmp\\\"quoted\\\"")
    var t212 string = "" + "}"
    _goml_runtime_core_string_println(t212)
    var t213 Option__int = Some{
        _0: 11,
    }
    var t214 int = unwrap_or_negative(t213)
    var t215 string = _goml_m_inherent_i_int_i_int_i_to__string(t214)
    println__T_string(t215)
    var t216 int = unwrap_or_negative(None{})
    var t217 string = _goml_m_inherent_i_int_i_int_i_to__string(t216)
    println__T_string(t217)
    var t218 int = count_to(4)
    var t219 string = _goml_m_inherent_i_int_i_int_i_to__string(t218)
    println__T_string(t219)
    var t220 Option__int = Some{
        _0: 9,
    }
    var t221 int = loop_option(t220)
    var t222 string = _goml_m_inherent_i_int_i_int_i_to__string(t221)
    println__T_string(t222)
    var t223 int = loop_option(None{})
    var t224 string = _goml_m_inherent_i_int_i_int_i_to__string(t223)
    println__T_string(t224)
    var t225 int = nested_loop_value()
    var t226 string = _goml_m_inherent_i_int_i_int_i_to__string(t225)
    println__T_string(t226)
    var t227 bool = matches_raw_path("C:\\tmp")
    var t228 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t227)
    println__T_string(t228)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv230 *ref_int_x
    var t231 *ref_int_x = ref__Ref_3int(value__207)
    retv230 = t231
    return retv230
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv233 int
    var t234 int = ref_get__Ref_3int(self__208)
    retv233 = t234
    return retv233
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t238)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv241 string
    var t242 string = _goml_runtime_core_int_to_string(self__5)
    retv241 = t242
    return retv241
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv244 string
    var t245 string = _goml_runtime_core_bool_to_string(self__37)
    retv244 = t245
    return retv244
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv247 string
    retv247 = self__38
    return retv247
}

func main() {
    main0()
}
