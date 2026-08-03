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
    var inline226 int32 = 0
    var inline227 *ref_int32_x = ref__Ref_5int32(inline226)
    sum__2 = inline227
    var i__3 *ref_int32_x
    var inline223 int32 = 0
    var inline224 *ref_int32_x = ref__Ref_5int32(inline223)
    i__3 = inline224
    Loop_loop157:
    for {
        var t158 int32
        var inline219 int32 = ref_get__Ref_5int32(i__3)
        t158 = inline219
        var t159 bool = t158 < limit__1
        if t159 {
            var cur__4 int32
            var inline217 int32 = ref_get__Ref_5int32(i__3)
            cur__4 = inline217
            var t160 int32 = cur__4 + 1
            ref_set__Ref_5int32(i__3, t160)
            var t166 bool
            var inline212 int32 = 1
            var inline213 bool = cur__4 == inline212
            t166 = inline213
            if t166 {
                continue
            } else {
                var mtmp138 Option__int32
                var inline208 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(cur__4, 2)
                if inline208 {
                    mtmp138 = None{}
                } else {
                    var inline209 int32 = cur__4 + 10
                    var inline210 Option__int32 = Some{
                        _0: inline209,
                    }
                    mtmp138 = inline210
                }
                var jp163 int32
                switch mtmp138.(type) {
                case None:
                    return None{}
                case Some:
                    var x139 int32 = mtmp138.(Some)._0
                    jp163 = x139
                    var t164 int32
                    var inline206 int32 = ref_get__Ref_5int32(sum__2)
                    t164 = inline206
                    var t165 int32 = t164 + jp163
                    ref_set__Ref_5int32(sum__2, t165)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop157
        }
    }
    var t155 int32
    var inline221 int32 = ref_get__Ref_5int32(sum__2)
    t155 = inline221
    var t156 Option__int32 = Some{
        _0: t155,
    }
    return t156
}

func main0() struct{} {
    var t174 Option__int32 = accumulate(2)
    var t175 string
    switch t174.(type) {
    case None:
        t175 = "none"
    case Some:
        var inline242 int32 = t174.(Some)._0
        var inline244 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline242)
        var inline245 string = "some=" + inline244
        t175 = inline245
    default:
        panic("non-exhaustive match")
    }
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t175)
    _goml_runtime_core_string_println(inline239)
    var t176 Option__int32 = accumulate(4)
    var t177 string
    switch t176.(type) {
    case None:
        t177 = "none"
    case Some:
        var inline234 int32 = t176.(Some)._0
        var inline236 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline234)
        var inline237 string = "some=" + inline236
        t177 = inline237
    default:
        panic("non-exhaustive match")
    }
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline231)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__94 int32, other__95 int32) bool {
    var t180 bool = self__94 == other__95
    return t180
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t191 string = _goml_runtime_core_int32_to_string(self__35)
    return t191
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
