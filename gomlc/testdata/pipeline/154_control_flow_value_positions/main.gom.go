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
    var sum__1 *ref_int_x
    var inline255 int = 0
    var inline256 *ref_int_x = ref__Ref_3int(inline255)
    sum__1 = inline256
    Loop_loop177:
    for {
        var t178 int
        var inline225 int = ref_get__Ref_3int(i__0)
        t178 = inline225
        var t179 bool = t178 < 5
        if t179 {
            var t180 int
            var inline223 int = ref_get__Ref_3int(i__0)
            t180 = inline223
            var t181 int = t180 + 1
            ref_set__Ref_3int(i__0, t181)
            var t186 int
            var inline219 int = ref_get__Ref_3int(i__0)
            t186 = inline219
            var t187 bool
            var inline216 int = 3
            var inline217 bool = t186 == inline216
            t187 = inline217
            var jp183 int
            if t187 {
                continue
            } else {
                var inline210 int = ref_get__Ref_3int(i__0)
                jp183 = inline210
                var t184 int
                var inline214 int = ref_get__Ref_3int(sum__1)
                t184 = inline214
                var t185 int = t184 + jp183
                ref_set__Ref_3int(sum__1, t185)
                continue
            }
        } else {
            break Loop_loop177
        }
    }
    var t166 int
    var inline253 int = ref_get__Ref_3int(sum__1)
    t166 = inline253
    var inline250 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t166)
    _goml_runtime_core_string_println(inline250)
    var j__3 *ref_int_x
    var inline247 int = 0
    var inline248 *ref_int_x = ref__Ref_3int(inline247)
    j__3 = inline248
    var total__4 *ref_int_x
    var inline244 int = 0
    var inline245 *ref_int_x = ref__Ref_3int(inline244)
    total__4 = inline245
    Loop_loop169:
    for {
        var t170 int
        var inline237 int = ref_get__Ref_3int(j__3)
        t170 = inline237
        var t171 int = t170 + 1
        ref_set__Ref_3int(j__3, t171)
        var mtmp160 int
        var inline233 int = ref_get__Ref_3int(j__3)
        mtmp160 = inline233
        var jp173 int
        switch mtmp160 {
        case 5:
            break Loop_loop169
        default:
            var inline227 int = ref_get__Ref_3int(j__3)
            jp173 = inline227
            var t174 int
            var inline231 int = ref_get__Ref_3int(total__4)
            t174 = inline231
            var t175 int = t174 + jp173
            ref_set__Ref_3int(total__4, t175)
            continue
        }
    }
    var t168 int
    var inline242 int = ref_get__Ref_3int(total__4)
    t168 = inline242
    var inline239 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t168)
    _goml_runtime_core_string_println(inline239)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t205 string = _goml_runtime_core_int_to_string(self__40)
    return t205
}

func main() {
    main0()
}
