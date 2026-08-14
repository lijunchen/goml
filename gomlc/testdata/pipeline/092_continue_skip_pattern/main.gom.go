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
    var i__0 *ref_int_x
    var inline258 int = 0
    var inline259 *ref_int_x = ref__Ref_3int(inline258)
    i__0 = inline259
    Loop_loop195:
    for {
        var t196 int
        var inline252 int = ref_get__Ref_3int(i__0)
        t196 = inline252
        var t197 bool = t196 < 8
        if t197 {
            var t198 int
            var inline250 int = ref_get__Ref_3int(i__0)
            t198 = inline250
            var t199 int = t198 + 1
            ref_set__Ref_3int(i__0, t199)
            var t205 int
            var inline246 int = ref_get__Ref_3int(i__0)
            t205 = inline246
            var t206 bool
            var inline243 int = 3
            var inline244 bool = t205 == inline243
            t206 = inline244
            if t206 {
                continue
            } else {
                var t203 int
                var inline241 int = ref_get__Ref_3int(i__0)
                t203 = inline241
                var t204 bool
                var inline238 int = 6
                var inline239 bool = t203 == inline238
                t204 = inline239
                if t204 {
                    continue
                } else {
                    var t202 int
                    var inline236 int = ref_get__Ref_3int(i__0)
                    t202 = inline236
                    var inline233 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t202)
                    _goml_runtime_core_string_println(inline233)
                    continue
                }
            }
        } else {
            break Loop_loop195
        }
    }
    var inline254 string = "done"
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline254)
    _goml_runtime_core_string_println(inline255)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t226 string = _goml_runtime_core_int_to_string(self__67)
    return t226
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
