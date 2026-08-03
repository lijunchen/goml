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
        var t144 Option__int32 = Some{
            _0: 41,
        }
        return t144
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t157 Option__int32
    var inline199 bool = true
    var inline200 Option__int32 = maybe_value(inline199)
    var inline202 int32
    switch inline200.(type) {
    case Some:
        var inline206 int32 = inline200.(Some)._0
        inline202 = inline206
        var inline204 int32 = inline202 + 1
        var inline205 Option__int32 = Some{
            _0: inline204,
        }
        t157 = inline205
        var t158 string
        switch t157.(type) {
        case Some:
            var inline195 int32 = t157.(Some)._0
            var inline197 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline195)
            t158 = inline197
        case None:
            t158 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t158)
        _goml_runtime_core_string_println(inline192)
        var t159 Option__int32
        var inline182 bool = false
        var inline183 Option__int32 = maybe_value(inline182)
        var inline185 int32
        switch inline183.(type) {
        case Some:
            var inline189 int32 = inline183.(Some)._0
            inline185 = inline189
            var inline187 int32 = inline185 + 1
            var inline188 Option__int32 = Some{
                _0: inline187,
            }
            t159 = inline188
            var t160 string
            switch t159.(type) {
            case Some:
                var inline178 int32 = t159.(Some)._0
                var inline180 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline178)
                t160 = inline180
            case None:
                t160 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
            _goml_runtime_core_string_println(inline175)
            return struct{}{}
        case None:
            t159 = None{}
            var t160 string
            switch t159.(type) {
            case Some:
                var inline178 int32 = t159.(Some)._0
                var inline180 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline178)
                t160 = inline180
            case None:
                t160 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
            _goml_runtime_core_string_println(inline175)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case None:
        t157 = None{}
        var t158 string
        switch t157.(type) {
        case Some:
            var inline195 int32 = t157.(Some)._0
            var inline197 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline195)
            t158 = inline197
        case None:
            t158 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t158)
        _goml_runtime_core_string_println(inline192)
        var t159 Option__int32
        var inline182 bool = false
        var inline183 Option__int32 = maybe_value(inline182)
        var inline185 int32
        switch inline183.(type) {
        case Some:
            var inline189 int32 = inline183.(Some)._0
            inline185 = inline189
            var inline187 int32 = inline185 + 1
            var inline188 Option__int32 = Some{
                _0: inline187,
            }
            t159 = inline188
            var t160 string
            switch t159.(type) {
            case Some:
                var inline178 int32 = t159.(Some)._0
                var inline180 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline178)
                t160 = inline180
            case None:
                t160 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
            _goml_runtime_core_string_println(inline175)
            return struct{}{}
        case None:
            t159 = None{}
            var t160 string
            switch t159.(type) {
            case Some:
                var inline178 int32 = t159.(Some)._0
                var inline180 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline178)
                t160 = inline180
            case None:
                t160 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
            _goml_runtime_core_string_println(inline175)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t164 string = _goml_runtime_core_int32_to_string(self__35)
    return t164
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
