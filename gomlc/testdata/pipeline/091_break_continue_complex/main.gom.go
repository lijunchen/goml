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
    Loop_loop182:
    for {
        var t183 int
        var inline234 int = ref_get__Ref_3int(i__1)
        t183 = inline234
        var t184 bool = t183 <= 100
        if t184 {
            var t191 int
            var inline232 int = ref_get__Ref_3int(i__1)
            t191 = inline232
            var t192 bool
            var inline229 int = 50
            var inline230 bool = t191 == inline229
            t192 = inline230
            if t192 {
                break Loop_loop182
            } else {
                var t186 int
                var inline227 int = ref_get__Ref_3int(sum__0)
                t186 = inline227
                var t187 int
                var inline225 int = ref_get__Ref_3int(i__1)
                t187 = inline225
                var t188 int = t186 + t187
                ref_set__Ref_3int(sum__0, t188)
                var t189 int
                var inline221 int = ref_get__Ref_3int(i__1)
                t189 = inline221
                var t190 int = t189 + 1
                ref_set__Ref_3int(i__1, t190)
                continue
            }
        } else {
            break Loop_loop182
        }
    }
    var inline268 string = "sum up to break: "
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline268)
    _goml_runtime_core_string_print(inline269)
    var t169 int
    var inline266 int = ref_get__Ref_3int(sum__0)
    t169 = inline266
    var inline263 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t169)
    _goml_runtime_core_string_println(inline263)
    var even_sum__2 *ref_int_x
    var inline260 int = 0
    var inline261 *ref_int_x = ref__Ref_3int(inline260)
    even_sum__2 = inline261
    var j__3 *ref_int_x
    var inline257 int = 1
    var inline258 *ref_int_x = ref__Ref_3int(inline257)
    j__3 = inline258
    Loop_loop172:
    for {
        var t173 int
        var inline246 int = ref_get__Ref_3int(j__3)
        t173 = inline246
        var t174 bool = t173 <= 10
        if t174 {
            var cur__4 int
            var inline244 int = ref_get__Ref_3int(j__3)
            cur__4 = inline244
            var t175 int = cur__4 + 1
            ref_set__Ref_3int(j__3, t175)
            var t177 int = cur__4 / 2
            var t178 int = t177 * 2
            var t179 bool
            var inline240 bool = cur__4 == t178
            t179 = inline240
            if t179 {
                var t180 int
                var inline238 int = ref_get__Ref_3int(even_sum__2)
                t180 = inline238
                var t181 int = t180 + cur__4
                ref_set__Ref_3int(even_sum__2, t181)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop172
        }
    }
    var inline253 string = "even sum: "
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline253)
    _goml_runtime_core_string_print(inline254)
    var t171 int
    var inline251 int = ref_get__Ref_3int(even_sum__2)
    t171 = inline251
    var inline248 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t171)
    _goml_runtime_core_string_println(inline248)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t195 *ref_int_x = ref__Ref_3int(value__207)
    return t195
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t214 string = _goml_runtime_core_int_to_string(self__40)
    return t214
}

func main() {
    main0()
}
