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
    Loop_loop184:
    for {
        var t195 int
        var inline248 int = ref_get__Ref_3int(i__0)
        t195 = inline248
        var t196 bool
        var inline245 int = 0
        var inline246 bool = t195 == inline245
        t196 = inline246
        var jp186 bool
        if t196 {
            var inline222 int = 1
            ref_set__Ref_3int(i__0, inline222)
            jp186 = true
        } else {
            var t199 int
            var inline225 int = ref_get__Ref_3int(i__0)
            t199 = inline225
            var t200 bool = t199 < 4
            if t200 {
                jp186 = true
            } else {
                jp186 = false
            }
        }
        if jp186 {
            var t187 int
            var inline243 int = ref_get__Ref_3int(total__1)
            t187 = inline243
            var t188 int
            var inline241 int = ref_get__Ref_3int(i__0)
            t188 = inline241
            var t189 int = t187 + t188
            ref_set__Ref_3int(total__1, t189)
            var t193 int
            var inline237 int = ref_get__Ref_3int(i__0)
            t193 = inline237
            var t194 bool
            var inline234 int = 1
            var inline235 bool = t193 == inline234
            t194 = inline235
            if t194 {
                var inline227 int = 2
                ref_set__Ref_3int(i__0, inline227)
                continue
            } else {
                var t191 int
                var inline232 int = ref_get__Ref_3int(i__0)
                t191 = inline232
                var t192 int = t191 + 1
                ref_set__Ref_3int(i__0, t192)
                continue
            }
        } else {
            break Loop_loop184
        }
    }
    var t172 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
    println__T_int(t172)
    var j__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total2__3 *ref_int_x
    var inline274 int = 0
    var inline275 *ref_int_x = ref__Ref_3int(inline274)
    total2__3 = inline275
    Loop_loop175:
    for {
        var mtmp162 int
        var inline267 int = ref_get__Ref_3int(j__2)
        mtmp162 = inline267
        var jp177 bool
        switch mtmp162 {
        case 0:
            var inline250 int = 1
            ref_set__Ref_3int(j__2, inline250)
            jp177 = true
        case 1:
            var inline253 int = 2
            ref_set__Ref_3int(j__2, inline253)
            jp177 = true
        case 2:
            jp177 = true
        default:
            jp177 = false
        }
        if jp177 {
            var t178 int
            var inline265 int = ref_get__Ref_3int(total2__3)
            t178 = inline265
            var t179 int
            var inline263 int = ref_get__Ref_3int(j__2)
            t179 = inline263
            var t180 int = t178 + t179
            ref_set__Ref_3int(total2__3, t180)
            var t182 int
            var inline259 int = ref_get__Ref_3int(j__2)
            t182 = inline259
            var t183 bool
            var inline256 int = 2
            var inline257 bool = t182 == inline256
            t183 = inline257
            if t183 {
                break Loop_loop175
            } else {
                continue
            }
        } else {
            break Loop_loop175
        }
    }
    var t174 int
    var inline272 int = ref_get__Ref_3int(total2__3)
    t174 = inline272
    var inline269 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t174)
    _goml_runtime_core_string_println(inline269)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t203 *ref_int_x = ref__Ref_3int(value__207)
    return t203
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t206 int = ref_get__Ref_3int(self__208)
    return t206
}

func println__T_int(value__1 int) struct{} {
    var t213 string
    var inline277 string = _goml_runtime_core_int_to_string(value__1)
    t213 = inline277
    _goml_runtime_core_string_println(t213)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t217 string = _goml_runtime_core_int_to_string(self__40)
    return t217
}

func main() {
    main0()
}
