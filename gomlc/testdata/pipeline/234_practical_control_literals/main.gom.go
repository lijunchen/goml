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
    var retv175 int
    var mtmp152 Option__int = value__0
    var jp177 int
    switch mtmp152.(type) {
    case Some:
        var x153 int = mtmp152.(Some)._0
        var result__1 int = x153
        jp177 = result__1
        retv175 = jp177
        return retv175
    default:
        retv175 = -1
        return retv175
    }
}

func count_to(limit__2 int) int {
    var retv179 int
    var counter__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var jp181 int
    Loop_loop_expr182:
    for {
        var current__4 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(counter__3)
        var t185 bool = current__4 >= limit__2
        if t185 {
            jp181 = current__4
            break Loop_loop_expr182
        } else {
            var t184 int = current__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(counter__3, t184)
            continue
        }
    }
    retv179 = jp181
    return retv179
}

func loop_option(value__5 Option__int) int {
    var retv187 int
    var jp189 int
    var mtmp158 Option__int = value__5
    switch mtmp158.(type) {
    case Some:
        var x159 int = mtmp158.(Some)._0
        var item__6 int = x159
        jp189 = item__6
        retv187 = jp189
        return retv187
    default:
        jp189 = -2
        retv187 = jp189
        return retv187
    }
}

func nested_loop_value() int {
    var retv193 int
    var jp195 int
    jp195 = 7
    retv193 = jp195
    return retv193
}

func matches_raw_path(value__7 string) bool {
    var retv205 bool
    var jp207 bool
    switch value__7 {
    case "C:\\tmp":
        jp207 = true
    default:
        jp207 = false
    }
    retv205 = jp207
    return retv205
}

func main0() struct{} {
    _goml_runtime_core_string_println("C:\\tmp\\\"quoted\\\"")
    var t209 string = "" + "}"
    _goml_runtime_core_string_println(t209)
    var t210 Option__int = Some{
        _0: 11,
    }
    var t211 int = unwrap_or_negative(t210)
    var t212 string = _goml_m_inherent_i_int_i_int_i_to__string(t211)
    println__T_string(t212)
    var t213 int = unwrap_or_negative(None{})
    var t214 string = _goml_m_inherent_i_int_i_int_i_to__string(t213)
    println__T_string(t214)
    var t215 int = count_to(4)
    var t216 string = _goml_m_inherent_i_int_i_int_i_to__string(t215)
    println__T_string(t216)
    var t217 Option__int = Some{
        _0: 9,
    }
    var t218 int = loop_option(t217)
    var t219 string = _goml_m_inherent_i_int_i_int_i_to__string(t218)
    println__T_string(t219)
    var t220 int = loop_option(None{})
    var t221 string = _goml_m_inherent_i_int_i_int_i_to__string(t220)
    println__T_string(t221)
    var t222 int = nested_loop_value()
    var t223 string = _goml_m_inherent_i_int_i_int_i_to__string(t222)
    println__T_string(t223)
    var t224 bool = matches_raw_path("C:\\tmp")
    var t225 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t224)
    println__T_string(t225)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv227 *ref_int_x
    var t228 *ref_int_x = ref__Ref_3int(value__207)
    retv227 = t228
    return retv227
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv230 int
    var t231 int = ref_get__Ref_3int(self__208)
    retv230 = t231
    return retv230
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t235)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv238 string
    var t239 string = _goml_runtime_core_int_to_string(self__5)
    retv238 = t239
    return retv238
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv241 string
    var t242 string = _goml_runtime_core_bool_to_string(self__37)
    retv241 = t242
    return retv241
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv244 string
    retv244 = self__38
    return retv244
}

func main() {
    main0()
}
