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
    var inline226 int = 0
    var inline227 *ref_int_x = ref__Ref_3int(inline226)
    i__0 = inline227
    Loop_loop163:
    for {
        var t164 int
        var inline220 int = ref_get__Ref_3int(i__0)
        t164 = inline220
        var t165 bool = t164 < 8
        if t165 {
            var t166 int
            var inline218 int = ref_get__Ref_3int(i__0)
            t166 = inline218
            var t167 int = t166 + 1
            ref_set__Ref_3int(i__0, t167)
            var t173 int
            var inline214 int = ref_get__Ref_3int(i__0)
            t173 = inline214
            var t174 bool
            var inline211 int = 3
            var inline212 bool = t173 == inline211
            t174 = inline212
            if t174 {
                continue
            } else {
                var t171 int
                var inline209 int = ref_get__Ref_3int(i__0)
                t171 = inline209
                var t172 bool
                var inline206 int = 6
                var inline207 bool = t171 == inline206
                t172 = inline207
                if t172 {
                    continue
                } else {
                    var t170 int
                    var inline204 int = ref_get__Ref_3int(i__0)
                    t170 = inline204
                    var inline201 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t170)
                    _goml_runtime_core_string_println(inline201)
                    continue
                }
            }
        } else {
            break Loop_loop163
        }
    }
    var inline222 string = "done"
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline222)
    _goml_runtime_core_string_println(inline223)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t194 string = _goml_runtime_core_int_to_string(self__40)
    return t194
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
