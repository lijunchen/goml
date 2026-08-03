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

type Mode int32

const (
    Take Mode = 0
    Skip Mode = 1
)

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var jp193 int32
    if top__1 {
        switch mode__2 {
        case Take:
            var mtmp177 Option__int32
            if inner_flag__3 {
                var inline225 Option__int32 = Some{
                    _0: 8,
                }
                mtmp177 = inline225
            } else {
                mtmp177 = None{}
            }
            var jp198 int32
            switch mtmp177.(type) {
            case None:
                return None{}
            case Some:
                var x178 int32 = mtmp177.(Some)._0
                jp198 = x178
                var t199 int32 = jp198 + 1
                jp193 = t199
                var t194 Option__int32 = Some{
                    _0: jp193,
                }
                return t194
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp193 = 20
            var t194 Option__int32 = Some{
                _0: jp193,
            }
            return t194
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp179 Option__int32
        if inner_flag__3 {
            var inline227 Option__int32 = Some{
                _0: 8,
            }
            mtmp179 = inline227
        } else {
            mtmp179 = None{}
        }
        var jp201 int32
        switch mtmp179.(type) {
        case None:
            return None{}
        case Some:
            var x180 int32 = mtmp179.(Some)._0
            jp201 = x180
            var t202 int32 = jp201 + 2
            jp193 = t202
            var t194 Option__int32 = Some{
                _0: jp193,
            }
            return t194
        default:
            panic("non-exhaustive match")
        }
    }
}

func main0() struct{} {
    var t210 Option__int32 = nested(true, Take, true)
    var t211 string
    switch t210.(type) {
    case None:
        t211 = "none"
    case Some:
        var inline250 int32 = t210.(Some)._0
        var inline252 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline250)
        var inline253 string = "some=" + inline252
        t211 = inline253
    default:
        panic("non-exhaustive match")
    }
    var inline247 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline247)
    var t212 Option__int32 = nested(true, Skip, false)
    var t213 string
    switch t212.(type) {
    case None:
        t213 = "none"
    case Some:
        var inline242 int32 = t212.(Some)._0
        var inline244 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline242)
        var inline245 string = "some=" + inline244
        t213 = inline245
    default:
        panic("non-exhaustive match")
    }
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline239)
    var t214 Option__int32 = nested(false, Take, false)
    var t215 string
    switch t214.(type) {
    case None:
        t215 = "none"
    case Some:
        var inline234 int32 = t214.(Some)._0
        var inline236 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline234)
        var inline237 string = "some=" + inline236
        t215 = inline237
    default:
        panic("non-exhaustive match")
    }
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline231)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t218 string = _goml_runtime_core_int32_to_string(self__35)
    return t218
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
