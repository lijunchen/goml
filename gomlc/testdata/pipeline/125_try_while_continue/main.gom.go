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
    var inline262 int32 = 0
    var inline263 *ref_int32_x = ref__Ref_5int32(inline262)
    sum__2 = inline263
    var i__3 *ref_int32_x
    var inline259 int32 = 0
    var inline260 *ref_int32_x = ref__Ref_5int32(inline259)
    i__3 = inline260
    Loop_loop193:
    for {
        var t194 int32
        var inline255 int32 = ref_get__Ref_5int32(i__3)
        t194 = inline255
        var t195 bool = t194 < limit__1
        if t195 {
            var cur__4 int32
            var inline253 int32 = ref_get__Ref_5int32(i__3)
            cur__4 = inline253
            var t196 int32 = cur__4 + 1
            ref_set__Ref_5int32(i__3, t196)
            var t202 bool
            var inline248 int32 = 1
            var inline249 bool = cur__4 == inline248
            t202 = inline249
            if t202 {
                continue
            } else {
                var mtmp174 Option__int32
                var inline244 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(cur__4, 2)
                if inline244 {
                    mtmp174 = None{}
                } else {
                    var inline245 int32 = cur__4 + 10
                    var inline246 Option__int32 = Some{
                        _0: inline245,
                    }
                    mtmp174 = inline246
                }
                var jp199 int32
                switch mtmp174.(type) {
                case None:
                    return None{}
                case Some:
                    var x175 int32 = mtmp174.(Some)._0
                    jp199 = x175
                    var t200 int32
                    var inline242 int32 = ref_get__Ref_5int32(sum__2)
                    t200 = inline242
                    var t201 int32 = t200 + jp199
                    ref_set__Ref_5int32(sum__2, t201)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop193
        }
    }
    var t191 int32
    var inline257 int32 = ref_get__Ref_5int32(sum__2)
    t191 = inline257
    var t192 Option__int32 = Some{
        _0: t191,
    }
    return t192
}

func main0() struct{} {
    var t210 Option__int32 = accumulate(2)
    var t211 string
    switch t210.(type) {
    case None:
        t211 = "none"
    case Some:
        var inline278 int32 = t210.(Some)._0
        var inline280 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline278)
        var inline281 string = "some=" + inline280
        t211 = inline281
    default:
        panic("non-exhaustive match")
    }
    var inline275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline275)
    var t212 Option__int32 = accumulate(4)
    var t213 string
    switch t212.(type) {
    case None:
        t213 = "none"
    case Some:
        var inline270 int32 = t212.(Some)._0
        var inline272 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline270)
        var inline273 string = "some=" + inline272
        t213 = inline273
    default:
        panic("non-exhaustive match")
    }
    var inline267 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline267)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(self__109 int32, other__110 int32) bool {
    var t216 bool = self__109 == other__110
    return t216
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t227 string = _goml_runtime_core_int32_to_string(self__35)
    return t227
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
