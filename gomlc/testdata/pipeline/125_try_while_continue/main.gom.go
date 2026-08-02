package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func accumulate(limit__1 int32) Option__int32 {
    var sum__2 *ref_int32_x
    var inline245 int32 = 0
    var inline246 *ref_int32_x = ref__Ref_5int32(inline245)
    sum__2 = inline246
    var i__3 *ref_int32_x
    var inline242 int32 = 0
    var inline243 *ref_int32_x = ref__Ref_5int32(inline242)
    i__3 = inline243
    Loop_loop176:
    for {
        var t177 int32
        var inline238 int32 = ref_get__Ref_5int32(i__3)
        t177 = inline238
        var t178 bool = t177 < limit__1
        if t178 {
            var cur__4 int32
            var inline236 int32 = ref_get__Ref_5int32(i__3)
            cur__4 = inline236
            var t179 int32 = cur__4 + 1
            ref_set__Ref_5int32(i__3, t179)
            var t185 bool
            var inline231 int32 = 1
            var inline232 bool = cur__4 == inline231
            t185 = inline232
            if t185 {
                continue
            } else {
                var mtmp157 Option__int32
                var inline227 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(cur__4, 2)
                if inline227 {
                    mtmp157 = None{}
                } else {
                    var inline228 int32 = cur__4 + 10
                    var inline229 Option__int32 = Some{
                        _0: inline228,
                    }
                    mtmp157 = inline229
                }
                var jp182 int32
                switch mtmp157.(type) {
                case None:
                    return None{}
                case Some:
                    var x158 int32 = mtmp157.(Some)._0
                    jp182 = x158
                    var t183 int32
                    var inline225 int32 = ref_get__Ref_5int32(sum__2)
                    t183 = inline225
                    var t184 int32 = t183 + jp182
                    ref_set__Ref_5int32(sum__2, t184)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop176
        }
    }
    var t174 int32
    var inline240 int32 = ref_get__Ref_5int32(sum__2)
    t174 = inline240
    var t175 Option__int32 = Some{
        _0: t174,
    }
    return t175
}

func main0() struct{} {
    var t193 Option__int32 = accumulate(2)
    var t194 string
    switch t193.(type) {
    case None:
        t194 = "none"
    case Some:
        var inline261 int32 = t193.(Some)._0
        var inline263 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline261)
        var inline264 string = "some=" + inline263
        t194 = inline264
    default:
        panic("non-exhaustive match")
    }
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline258)
    var t195 Option__int32 = accumulate(4)
    var t196 string
    switch t195.(type) {
    case None:
        t196 = "none"
    case Some:
        var inline253 int32 = t195.(Some)._0
        var inline255 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline253)
        var inline256 string = "some=" + inline255
        t196 = inline256
    default:
        panic("non-exhaustive match")
    }
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline250)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var t199 bool = self__65 == other__66
    return t199
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t210 string = _goml_runtime_core_int32_to_string(self__6)
    return t210
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
