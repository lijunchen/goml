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

func maybe_value(flag__0 bool) Option__int32 {
    if flag__0 {
        var t145 Option__int32 = Some{
            _0: 4,
        }
        return t145
    } else {
        return None{}
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var t148 int32 = a__1 + b__2
    return t148
}

func main0() struct{} {
    var t162 Option__int32
    var inline207 bool = true
    var inline208 Option__int32 = maybe_value(inline207)
    var inline210 int32
    switch inline208.(type) {
    case None:
        t162 = None{}
        var t163 string
        switch t162.(type) {
        case None:
            t163 = "none"
        case Some:
            var inline202 int32 = t162.(Some)._0
            var inline204 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline202)
            var inline205 string = "some=" + inline204
            t163 = inline205
        default:
            panic("non-exhaustive match")
        }
        var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
        _goml_runtime_core_string_println(inline199)
        var t164 Option__int32
        var inline190 bool = false
        var inline191 Option__int32 = maybe_value(inline190)
        var inline193 int32
        switch inline191.(type) {
        case None:
            t164 = None{}
            var t165 string
            switch t164.(type) {
            case None:
                t165 = "none"
            case Some:
                var inline185 int32 = t164.(Some)._0
                var inline187 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline185)
                var inline188 string = "some=" + inline187
                t165 = inline188
            default:
                panic("non-exhaustive match")
            }
            var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
            _goml_runtime_core_string_println(inline182)
            return struct{}{}
        case Some:
            var inline196 int32 = inline191.(Some)._0
            inline193 = inline196
            var inline194 int32 = add(inline193, 2)
            var inline195 Option__int32 = Some{
                _0: inline194,
            }
            t164 = inline195
            var t165 string
            switch t164.(type) {
            case None:
                t165 = "none"
            case Some:
                var inline185 int32 = t164.(Some)._0
                var inline187 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline185)
                var inline188 string = "some=" + inline187
                t165 = inline188
            default:
                panic("non-exhaustive match")
            }
            var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
            _goml_runtime_core_string_println(inline182)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline213 int32 = inline208.(Some)._0
        inline210 = inline213
        var inline211 int32 = add(inline210, 2)
        var inline212 Option__int32 = Some{
            _0: inline211,
        }
        t162 = inline212
        var t163 string
        switch t162.(type) {
        case None:
            t163 = "none"
        case Some:
            var inline202 int32 = t162.(Some)._0
            var inline204 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline202)
            var inline205 string = "some=" + inline204
            t163 = inline205
        default:
            panic("non-exhaustive match")
        }
        var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
        _goml_runtime_core_string_println(inline199)
        var t164 Option__int32
        var inline190 bool = false
        var inline191 Option__int32 = maybe_value(inline190)
        var inline193 int32
        switch inline191.(type) {
        case None:
            t164 = None{}
            var t165 string
            switch t164.(type) {
            case None:
                t165 = "none"
            case Some:
                var inline185 int32 = t164.(Some)._0
                var inline187 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline185)
                var inline188 string = "some=" + inline187
                t165 = inline188
            default:
                panic("non-exhaustive match")
            }
            var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
            _goml_runtime_core_string_println(inline182)
            return struct{}{}
        case Some:
            var inline196 int32 = inline191.(Some)._0
            inline193 = inline196
            var inline194 int32 = add(inline193, 2)
            var inline195 Option__int32 = Some{
                _0: inline194,
            }
            t164 = inline195
            var t165 string
            switch t164.(type) {
            case None:
                t165 = "none"
            case Some:
                var inline185 int32 = t164.(Some)._0
                var inline187 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline185)
                var inline188 string = "some=" + inline187
                t165 = inline188
            default:
                panic("non-exhaustive match")
            }
            var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
            _goml_runtime_core_string_println(inline182)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t168 string = _goml_runtime_core_int32_to_string(self__35)
    return t168
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
