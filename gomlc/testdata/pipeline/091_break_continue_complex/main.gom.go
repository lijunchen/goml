package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
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

func main0() struct{} {
    var sum__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var i__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    Loop_loop163:
    for {
        var t164 int
        var inline215 int = ref_get__Ref_3int(i__1)
        t164 = inline215
        var t165 bool = t164 <= 100
        if t165 {
            var t172 int
            var inline213 int = ref_get__Ref_3int(i__1)
            t172 = inline213
            var t173 bool
            var inline210 int = 50
            var inline211 bool = t172 == inline210
            t173 = inline211
            if t173 {
                break Loop_loop163
            } else {
                var t167 int
                var inline208 int = ref_get__Ref_3int(sum__0)
                t167 = inline208
                var t168 int
                var inline206 int = ref_get__Ref_3int(i__1)
                t168 = inline206
                var t169 int = t167 + t168
                ref_set__Ref_3int(sum__0, t169)
                var t170 int
                var inline202 int = ref_get__Ref_3int(i__1)
                t170 = inline202
                var t171 int = t170 + 1
                ref_set__Ref_3int(i__1, t171)
                continue
            }
        } else {
            break Loop_loop163
        }
    }
    var inline249 string = "sum up to break: "
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline249)
    _goml_runtime_core_string_print(inline250)
    var t150 int
    var inline247 int = ref_get__Ref_3int(sum__0)
    t150 = inline247
    var inline244 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t150)
    _goml_runtime_core_string_println(inline244)
    var even_sum__2 *ref_int_x
    var inline241 int = 0
    var inline242 *ref_int_x = ref__Ref_3int(inline241)
    even_sum__2 = inline242
    var j__3 *ref_int_x
    var inline238 int = 1
    var inline239 *ref_int_x = ref__Ref_3int(inline238)
    j__3 = inline239
    Loop_loop153:
    for {
        var t154 int
        var inline227 int = ref_get__Ref_3int(j__3)
        t154 = inline227
        var t155 bool = t154 <= 10
        if t155 {
            var cur__4 int
            var inline225 int = ref_get__Ref_3int(j__3)
            cur__4 = inline225
            var t156 int = cur__4 + 1
            ref_set__Ref_3int(j__3, t156)
            var t158 int = cur__4 / 2
            var t159 int = t158 * 2
            var t160 bool
            var inline221 bool = cur__4 == t159
            t160 = inline221
            if t160 {
                var t161 int
                var inline219 int = ref_get__Ref_3int(even_sum__2)
                t161 = inline219
                var t162 int = t161 + cur__4
                ref_set__Ref_3int(even_sum__2, t162)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop153
        }
    }
    var inline234 string = "even sum: "
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline234)
    _goml_runtime_core_string_print(inline235)
    var t152 int
    var inline232 int = ref_get__Ref_3int(even_sum__2)
    t152 = inline232
    var inline229 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t152)
    _goml_runtime_core_string_println(inline229)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__215 int) *ref_int_x {
    var t176 *ref_int_x = ref__Ref_3int(value__215)
    return t176
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t195 string = _goml_runtime_core_int_to_string(self__69)
    return t195
}

func main() {
    main0()
}
