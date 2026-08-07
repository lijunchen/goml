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
    var i__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop165:
    for {
        var t176 int
        var inline229 int = ref_get__Ref_3int(i__0)
        t176 = inline229
        var t177 bool
        var inline226 int = 0
        var inline227 bool = t176 == inline226
        t177 = inline227
        var jp167 bool
        if t177 {
            var inline203 int = 1
            ref_set__Ref_3int(i__0, inline203)
            jp167 = true
        } else {
            var t180 int
            var inline206 int = ref_get__Ref_3int(i__0)
            t180 = inline206
            var t181 bool = t180 < 4
            if t181 {
                jp167 = true
            } else {
                jp167 = false
            }
        }
        if jp167 {
            var t168 int
            var inline224 int = ref_get__Ref_3int(total__1)
            t168 = inline224
            var t169 int
            var inline222 int = ref_get__Ref_3int(i__0)
            t169 = inline222
            var t170 int = t168 + t169
            ref_set__Ref_3int(total__1, t170)
            var t174 int
            var inline218 int = ref_get__Ref_3int(i__0)
            t174 = inline218
            var t175 bool
            var inline215 int = 1
            var inline216 bool = t174 == inline215
            t175 = inline216
            if t175 {
                var inline208 int = 2
                ref_set__Ref_3int(i__0, inline208)
                continue
            } else {
                var t172 int
                var inline213 int = ref_get__Ref_3int(i__0)
                t172 = inline213
                var t173 int = t172 + 1
                ref_set__Ref_3int(i__0, t173)
                continue
            }
        } else {
            break Loop_loop165
        }
    }
    var t153 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
    println__T_int(t153)
    var j__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total2__3 *ref_int_x
    var inline255 int = 0
    var inline256 *ref_int_x = ref__Ref_3int(inline255)
    total2__3 = inline256
    Loop_loop156:
    for {
        var mtmp143 int
        var inline248 int = ref_get__Ref_3int(j__2)
        mtmp143 = inline248
        var jp158 bool
        switch mtmp143 {
        case 0:
            var inline231 int = 1
            ref_set__Ref_3int(j__2, inline231)
            jp158 = true
        case 1:
            var inline234 int = 2
            ref_set__Ref_3int(j__2, inline234)
            jp158 = true
        case 2:
            jp158 = true
        default:
            jp158 = false
        }
        if jp158 {
            var t159 int
            var inline246 int = ref_get__Ref_3int(total2__3)
            t159 = inline246
            var t160 int
            var inline244 int = ref_get__Ref_3int(j__2)
            t160 = inline244
            var t161 int = t159 + t160
            ref_set__Ref_3int(total2__3, t161)
            var t163 int
            var inline240 int = ref_get__Ref_3int(j__2)
            t163 = inline240
            var t164 bool
            var inline237 int = 2
            var inline238 bool = t163 == inline237
            t164 = inline238
            if t164 {
                break Loop_loop156
            } else {
                continue
            }
        } else {
            break Loop_loop156
        }
    }
    var t155 int
    var inline253 int = ref_get__Ref_3int(total2__3)
    t155 = inline253
    var inline250 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t155)
    _goml_runtime_core_string_println(inline250)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__232 int) *ref_int_x {
    var t184 *ref_int_x = ref__Ref_3int(value__232)
    return t184
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__233 *ref_int_x) int {
    var t187 int = ref_get__Ref_3int(self__233)
    return t187
}

func println__T_int(value__31 int) struct{} {
    var t194 string
    var inline258 string = _goml_runtime_core_int_to_string(value__31)
    t194 = inline258
    _goml_runtime_core_string_println(t194)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t198 string = _goml_runtime_core_int_to_string(self__69)
    return t198
}

func main() {
    main0()
}
