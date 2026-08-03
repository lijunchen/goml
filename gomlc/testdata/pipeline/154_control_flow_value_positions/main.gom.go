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
    var inline239 int = 0
    var inline240 *ref_int_x = ref__Ref_3int(inline239)
    i__0 = inline240
    var sum__1 *ref_int_x
    var inline236 int = 0
    var inline237 *ref_int_x = ref__Ref_3int(inline236)
    sum__1 = inline237
    Loop_loop158:
    for {
        var t159 int
        var inline206 int = ref_get__Ref_3int(i__0)
        t159 = inline206
        var t160 bool = t159 < 5
        if t160 {
            var t161 int
            var inline204 int = ref_get__Ref_3int(i__0)
            t161 = inline204
            var t162 int = t161 + 1
            ref_set__Ref_3int(i__0, t162)
            var t167 int
            var inline200 int = ref_get__Ref_3int(i__0)
            t167 = inline200
            var t168 bool
            var inline197 int = 3
            var inline198 bool = t167 == inline197
            t168 = inline198
            var jp164 int
            if t168 {
                continue
            } else {
                var inline191 int = ref_get__Ref_3int(i__0)
                jp164 = inline191
                var t165 int
                var inline195 int = ref_get__Ref_3int(sum__1)
                t165 = inline195
                var t166 int = t165 + jp164
                ref_set__Ref_3int(sum__1, t166)
                continue
            }
        } else {
            break Loop_loop158
        }
    }
    var t147 int
    var inline234 int = ref_get__Ref_3int(sum__1)
    t147 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t147)
    _goml_runtime_core_string_println(inline231)
    var j__3 *ref_int_x
    var inline228 int = 0
    var inline229 *ref_int_x = ref__Ref_3int(inline228)
    j__3 = inline229
    var total__4 *ref_int_x
    var inline225 int = 0
    var inline226 *ref_int_x = ref__Ref_3int(inline225)
    total__4 = inline226
    Loop_loop150:
    for {
        var t151 int
        var inline218 int = ref_get__Ref_3int(j__3)
        t151 = inline218
        var t152 int = t151 + 1
        ref_set__Ref_3int(j__3, t152)
        var mtmp141 int
        var inline214 int = ref_get__Ref_3int(j__3)
        mtmp141 = inline214
        var jp154 int
        switch mtmp141 {
        case 5:
            break Loop_loop150
        default:
            var inline208 int = ref_get__Ref_3int(j__3)
            jp154 = inline208
            var t155 int
            var inline212 int = ref_get__Ref_3int(total__4)
            t155 = inline212
            var t156 int = t155 + jp154
            ref_set__Ref_3int(total__4, t156)
            continue
        }
    }
    var t149 int
    var inline223 int = ref_get__Ref_3int(total__4)
    t149 = inline223
    var inline220 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t149)
    _goml_runtime_core_string_println(inline220)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t186 string = _goml_runtime_core_int_to_string(self__69)
    return t186
}

func main() {
    main0()
}
