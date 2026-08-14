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

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func maybe_total(flag__2 bool) Option__int32 {
    var mtmp182 Option__int32
    if flag__2 {
        var inline233 Option__int32 = Some{
            _0: 3,
        }
        mtmp182 = inline233
    } else {
        mtmp182 = None{}
    }
    var jp204 int32
    switch mtmp182.(type) {
    case None:
        return None{}
    case Some:
        var x183 int32 = mtmp182.(Some)._0
        jp204 = x183
        var mtmp184 Option__int32
        var inline229 bool = jp204 > 0
        if inline229 {
            var inline230 int32 = jp204 * 2
            var inline231 Option__int32 = Some{
                _0: inline230,
            }
            mtmp184 = inline231
        } else {
            mtmp184 = None{}
        }
        var jp206 int32
        switch mtmp184.(type) {
        case None:
            return None{}
        case Some:
            var x185 int32 = mtmp184.(Some)._0
            jp206 = x185
            var t207 int32 = jp204 + jp206
            var t208 Option__int32 = Some{
                _0: t207,
            }
            return t208
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t216 Option__int32 = maybe_total(true)
    var t217 string
    switch t216.(type) {
    case None:
        t217 = "none"
    case Some:
        var inline248 int32 = t216.(Some)._0
        var inline250 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline248)
        var inline251 string = "some=" + inline250
        t217 = inline251
    default:
        panic("non-exhaustive match")
    }
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline245)
    var t218 Option__int32 = maybe_total(false)
    var t219 string
    switch t218.(type) {
    case None:
        t219 = "none"
    case Some:
        var inline240 int32 = t218.(Some)._0
        var inline242 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline240)
        var inline243 string = "some=" + inline242
        t219 = inline243
    default:
        panic("non-exhaustive match")
    }
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline237)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t222 string = _goml_runtime_core_int32_to_string(self__33)
    return t222
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
