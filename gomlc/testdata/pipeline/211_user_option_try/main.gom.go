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
        var t163 Option__int32 = Some{
            _0: 41,
        }
        return t163
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t176 Option__int32
    var inline218 bool = true
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
        t176 = inline224
        var t177 string
        switch t176.(type) {
        case Some:
            var inline214 int32 = t176.(Some)._0
            var inline216 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline214)
            t177 = inline216
        case None:
            t177 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
        _goml_runtime_core_string_println(inline211)
        var t178 Option__int32
        var inline201 bool = false
        var inline202 Option__int32 = maybe_value(inline201)
        var inline204 int32
        switch inline202.(type) {
        case Some:
            var inline208 int32 = inline202.(Some)._0
            inline204 = inline208
            var inline206 int32 = inline204 + 1
            var inline207 Option__int32 = Some{
                _0: inline206,
            }
            t178 = inline207
            var t179 string
            switch t178.(type) {
            case Some:
                var inline197 int32 = t178.(Some)._0
                var inline199 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline197)
                t179 = inline199
            case None:
                t179 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
            _goml_runtime_core_string_println(inline194)
            return struct{}{}
        case None:
            t178 = None{}
            var t179 string
            switch t178.(type) {
            case Some:
                var inline197 int32 = t178.(Some)._0
                var inline199 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline197)
                t179 = inline199
            case None:
                t179 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
            _goml_runtime_core_string_println(inline194)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case None:
        t176 = None{}
        var t177 string
        switch t176.(type) {
        case Some:
            var inline214 int32 = t176.(Some)._0
            var inline216 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline214)
            t177 = inline216
        case None:
            t177 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
        _goml_runtime_core_string_println(inline211)
        var t178 Option__int32
        var inline201 bool = false
        var inline202 Option__int32 = maybe_value(inline201)
        var inline204 int32
        switch inline202.(type) {
        case Some:
            var inline208 int32 = inline202.(Some)._0
            inline204 = inline208
            var inline206 int32 = inline204 + 1
            var inline207 Option__int32 = Some{
                _0: inline206,
            }
            t178 = inline207
            var t179 string
            switch t178.(type) {
            case Some:
                var inline197 int32 = t178.(Some)._0
                var inline199 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline197)
                t179 = inline199
            case None:
                t179 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
            _goml_runtime_core_string_println(inline194)
            return struct{}{}
        case None:
            t178 = None{}
            var t179 string
            switch t178.(type) {
            case Some:
                var inline197 int32 = t178.(Some)._0
                var inline199 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline197)
                t179 = inline199
            case None:
                t179 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
            _goml_runtime_core_string_println(inline194)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t183 string = _goml_runtime_core_int32_to_string(self__6)
    return t183
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
