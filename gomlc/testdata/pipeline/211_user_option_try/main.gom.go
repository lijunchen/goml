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

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

type None struct {}

func (_ None) isOption__int32() {}

func maybe_value(flag__0 bool) Option__int32 {
    if flag__0 {
        var t180 Option__int32 = Some{
            _0: 41,
        }
        return t180
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t193 Option__int32
    var inline235 bool = true
    var inline236 Option__int32 = maybe_value(inline235)
    var inline238 int32
    switch inline236.(type) {
    case Some:
        var inline242 int32 = inline236.(Some)._0
        inline238 = inline242
        var inline240 int32 = inline238 + 1
        var inline241 Option__int32 = Some{
            _0: inline240,
        }
        t193 = inline241
        var t194 string
        switch t193.(type) {
        case Some:
            var inline231 int32 = t193.(Some)._0
            var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
            t194 = inline233
        case None:
            t194 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
        _goml_runtime_core_string_println(inline228)
        var t195 Option__int32
        var inline218 bool = false
        var inline219 Option__int32 = maybe_value(inline218)
        var inline221 int32
        switch inline219.(type) {
        case Some:
            var inline225 int32 = inline219.(Some)._0
            inline221 = inline225
            var inline223 int32 = inline221 + 1
            var inline224 Option__int32 = Some{
                _0: inline223,
            }
            t195 = inline224
            var t196 string
            switch t195.(type) {
            case Some:
                var inline214 int32 = t195.(Some)._0
                var inline216 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline214)
                t196 = inline216
            case None:
                t196 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
            _goml_runtime_core_string_println(inline211)
            return struct{}{}
        case None:
            t195 = None{}
            var t196 string
            switch t195.(type) {
            case Some:
                var inline214 int32 = t195.(Some)._0
                var inline216 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline214)
                t196 = inline216
            case None:
                t196 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
            _goml_runtime_core_string_println(inline211)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case None:
        t193 = None{}
        var t194 string
        switch t193.(type) {
        case Some:
            var inline231 int32 = t193.(Some)._0
            var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
            t194 = inline233
        case None:
            t194 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
        _goml_runtime_core_string_println(inline228)
        var t195 Option__int32
        var inline218 bool = false
        var inline219 Option__int32 = maybe_value(inline218)
        var inline221 int32
        switch inline219.(type) {
        case Some:
            var inline225 int32 = inline219.(Some)._0
            inline221 = inline225
            var inline223 int32 = inline221 + 1
            var inline224 Option__int32 = Some{
                _0: inline223,
            }
            t195 = inline224
            var t196 string
            switch t195.(type) {
            case Some:
                var inline214 int32 = t195.(Some)._0
                var inline216 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline214)
                t196 = inline216
            case None:
                t196 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
            _goml_runtime_core_string_println(inline211)
            return struct{}{}
        case None:
            t195 = None{}
            var t196 string
            switch t195.(type) {
            case Some:
                var inline214 int32 = t195.(Some)._0
                var inline216 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline214)
                t196 = inline216
            case None:
                t196 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
            _goml_runtime_core_string_println(inline211)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t200 string = _goml_runtime_core_int32_to_string(self__33)
    return t200
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
