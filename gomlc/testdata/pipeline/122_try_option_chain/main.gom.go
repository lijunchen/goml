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
    var mtmp136 Option__int32
    if flag__2 {
        var inline187 Option__int32 = Some{
            _0: 3,
        }
        mtmp136 = inline187
    } else {
        mtmp136 = None{}
    }
    var jp158 int32
    switch mtmp136.(type) {
    case None:
        return None{}
    case Some:
        var x137 int32 = mtmp136.(Some)._0
        jp158 = x137
        var mtmp138 Option__int32
        var inline183 bool = jp158 > 0
        if inline183 {
            var inline184 int32 = jp158 * 2
            var inline185 Option__int32 = Some{
                _0: inline184,
            }
            mtmp138 = inline185
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
            var t161 int32 = jp158 + jp160
            var t162 Option__int32 = Some{
                _0: t161,
            }
            return t162
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t170 Option__int32 = maybe_total(true)
    var t171 string
    switch t170.(type) {
    case None:
        t171 = "none"
    case Some:
        var inline202 int32 = t170.(Some)._0
        var inline204 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline202)
        var inline205 string = "some=" + inline204
        t171 = inline205
    default:
        panic("non-exhaustive match")
    }
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
    _goml_runtime_core_string_println(inline199)
    var t172 Option__int32 = maybe_total(false)
    var t173 string
    switch t172.(type) {
    case None:
        t173 = "none"
    case Some:
        var inline194 int32 = t172.(Some)._0
        var inline196 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline194)
        var inline197 string = "some=" + inline196
        t173 = inline197
    default:
        panic("non-exhaustive match")
    }
    var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t173)
    _goml_runtime_core_string_println(inline191)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t176 string = _goml_runtime_core_int32_to_string(self__35)
    return t176
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
