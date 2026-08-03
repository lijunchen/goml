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
    Loop_loop183:
    for {
        var t189 int
        var inline221 int = ref_get__Ref_3int(i__0)
        t189 = inline221
        var t190 bool = t189 < 3
        var jp185 bool
        if t190 {
            jp185 = true
        } else {
            jp185 = false
        }
        if jp185 {
            var t186 int
            var inline219 int = ref_get__Ref_3int(i__0)
            t186 = inline219
            var inline216 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t186)
            _goml_runtime_core_string_println(inline216)
            var t187 int
            var inline214 int = ref_get__Ref_3int(i__0)
            t187 = inline214
            var t188 int = t187 + 1
            ref_set__Ref_3int(i__0, t188)
            continue
        } else {
            break Loop_loop183
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop166:
    for {
        var t174 int
        var inline243 int = ref_get__Ref_3int(j__1)
        t174 = inline243
        var t175 bool = t174 < 4
        var jp168 bool
        if t175 {
            var t178 int
            var inline231 int = ref_get__Ref_3int(j__1)
            t178 = inline231
            var t179 bool
            var inline228 int = 1
            var inline229 bool = t178 == inline228
            t179 = inline229
            if t179 {
                jp168 = true
            } else {
                var t180 int
                var inline226 int = ref_get__Ref_3int(j__1)
                t180 = inline226
                var t181 bool
                var inline223 int = 3
                var inline224 bool = t180 == inline223
                t181 = inline224
                var t182 bool = !t181
                jp168 = t182
            }
        } else {
            jp168 = false
        }
        if jp168 {
            var t169 int
            var inline241 int = ref_get__Ref_3int(total__2)
            t169 = inline241
            var t170 int
            var inline239 int = ref_get__Ref_3int(j__1)
            t170 = inline239
            var t171 int = t169 + t170
            ref_set__Ref_3int(total__2, t171)
            var t172 int
            var inline235 int = ref_get__Ref_3int(j__1)
            t172 = inline235
            var t173 int = t172 + 1
            ref_set__Ref_3int(j__1, t173)
            continue
        } else {
            break Loop_loop166
        }
    }
    var t151 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
    println__T_int(t151)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop154:
    for {
        var mtmp143 int
        var inline260 int = ref_get__Ref_3int(k__3)
        mtmp143 = inline260
        var jp156 bool
        switch mtmp143 {
        case 0:
            jp156 = true
        case 1:
            var t164 int
            var inline248 int = ref_get__Ref_3int(sum__4)
            t164 = inline248
            var t165 bool
            var inline245 int = 0
            var inline246 bool = t164 == inline245
            t165 = inline246
            if t165 {
                jp156 = true
            } else {
                jp156 = false
            }
        case 2:
            jp156 = true
        default:
            jp156 = false
        }
        if jp156 {
            var t157 int
            var inline258 int = ref_get__Ref_3int(sum__4)
            t157 = inline258
            var t158 int
            var inline256 int = ref_get__Ref_3int(k__3)
            t158 = inline256
            var t159 int = t157 + t158
            ref_set__Ref_3int(sum__4, t159)
            var t160 int
            var inline252 int = ref_get__Ref_3int(k__3)
            t160 = inline252
            var t161 int = t160 + 1
            ref_set__Ref_3int(k__3, t161)
            continue
        } else {
            break Loop_loop154
        }
    }
    var t153 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
    println__T_int(t153)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__215 int) *ref_int_x {
    var t193 *ref_int_x = ref__Ref_3int(value__215)
    return t193
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__216 *ref_int_x) int {
    var t196 int = ref_get__Ref_3int(self__216)
    return t196
}

func println__T_int(value__31 int) struct{} {
    var t198 string
    var inline262 string = _goml_runtime_core_int_to_string(value__31)
    t198 = inline262
    _goml_runtime_core_string_println(t198)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t207 string = _goml_runtime_core_int_to_string(self__69)
    return t207
}

func main() {
    main0()
}
