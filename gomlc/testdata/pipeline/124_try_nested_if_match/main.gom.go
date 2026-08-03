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
    var jp152 int32
    if top__1 {
        switch mode__2 {
        case Take:
            var mtmp136 Option__int32
            if inner_flag__3 {
                var inline184 Option__int32 = Some{
                    _0: 8,
                }
                mtmp136 = inline184
            } else {
                mtmp136 = None{}
            }
            var jp157 int32
            switch mtmp136.(type) {
            case None:
                return None{}
            case Some:
                var x137 int32 = mtmp136.(Some)._0
                jp157 = x137
                var t158 int32 = jp157 + 1
                jp152 = t158
                var t153 Option__int32 = Some{
                    _0: jp152,
                }
                return t153
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp152 = 20
            var t153 Option__int32 = Some{
                _0: jp152,
            }
            return t153
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp138 Option__int32
        if inner_flag__3 {
            var inline186 Option__int32 = Some{
                _0: 8,
            }
            mtmp138 = inline186
        } else {
            mtmp138 = None{}
        }
        var jp160 int32
        switch mtmp138.(type) {
        case None:
            return None{}
        case Some:
            var x139 int32 = mtmp138.(Some)._0
            jp160 = x139
            var t161 int32 = jp160 + 2
            jp152 = t161
            var t153 Option__int32 = Some{
                _0: jp152,
            }
            return t153
        default:
            panic("non-exhaustive match")
        }
    }
}

func main0() struct{} {
    var t169 Option__int32 = nested(true, Take, true)
    var t170 string
    switch t169.(type) {
    case None:
        t170 = "none"
    case Some:
        var inline209 int32 = t169.(Some)._0
        var inline211 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline209)
        var inline212 string = "some=" + inline211
        t170 = inline212
    default:
        panic("non-exhaustive match")
    }
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t170)
    _goml_runtime_core_string_println(inline206)
    var t171 Option__int32 = nested(true, Skip, false)
    var t172 string
    switch t171.(type) {
    case None:
        t172 = "none"
    case Some:
        var inline201 int32 = t171.(Some)._0
        var inline203 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline201)
        var inline204 string = "some=" + inline203
        t172 = inline204
    default:
        panic("non-exhaustive match")
    }
    var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
    _goml_runtime_core_string_println(inline198)
    var t173 Option__int32 = nested(false, Take, false)
    var t174 string
    switch t173.(type) {
    case None:
        t174 = "none"
    case Some:
        var inline193 int32 = t173.(Some)._0
        var inline195 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline193)
        var inline196 string = "some=" + inline195
        t174 = inline196
    default:
        panic("non-exhaustive match")
    }
    var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline190)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t177 string = _goml_runtime_core_int32_to_string(self__35)
    return t177
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
