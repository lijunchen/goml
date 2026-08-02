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
        var t164 Option__int32 = Some{
            _0: 4,
        }
        return t164
    } else {
        return None{}
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var t167 int32 = a__1 + b__2
    return t167
}

func main0() struct{} {
    var t181 Option__int32
    var inline226 bool = true
    var inline227 Option__int32 = maybe_value(inline226)
    var inline229 int32
    switch inline227.(type) {
    case None:
        t181 = None{}
        var t182 string
        switch t181.(type) {
        case None:
            t182 = "none"
        case Some:
            var inline221 int32 = t181.(Some)._0
            var inline223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline221)
            var inline224 string = "some=" + inline223
            t182 = inline224
        default:
            panic("non-exhaustive match")
        }
        var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
        _goml_runtime_core_string_println(inline218)
        var t183 Option__int32
        var inline209 bool = false
        var inline210 Option__int32 = maybe_value(inline209)
        var inline212 int32
        switch inline210.(type) {
        case None:
            t183 = None{}
            var t184 string
            switch t183.(type) {
            case None:
                t184 = "none"
            case Some:
                var inline204 int32 = t183.(Some)._0
                var inline206 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline204)
                var inline207 string = "some=" + inline206
                t184 = inline207
            default:
                panic("non-exhaustive match")
            }
            var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
            _goml_runtime_core_string_println(inline201)
            return struct{}{}
        case Some:
            var inline215 int32 = inline210.(Some)._0
            inline212 = inline215
            var inline213 int32 = add(inline212, 2)
            var inline214 Option__int32 = Some{
                _0: inline213,
            }
            t183 = inline214
            var t184 string
            switch t183.(type) {
            case None:
                t184 = "none"
            case Some:
                var inline204 int32 = t183.(Some)._0
                var inline206 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline204)
                var inline207 string = "some=" + inline206
                t184 = inline207
            default:
                panic("non-exhaustive match")
            }
            var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
            _goml_runtime_core_string_println(inline201)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline232 int32 = inline227.(Some)._0
        inline229 = inline232
        var inline230 int32 = add(inline229, 2)
        var inline231 Option__int32 = Some{
            _0: inline230,
        }
        t181 = inline231
        var t182 string
        switch t181.(type) {
        case None:
            t182 = "none"
        case Some:
            var inline221 int32 = t181.(Some)._0
            var inline223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline221)
            var inline224 string = "some=" + inline223
            t182 = inline224
        default:
            panic("non-exhaustive match")
        }
        var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
        _goml_runtime_core_string_println(inline218)
        var t183 Option__int32
        var inline209 bool = false
        var inline210 Option__int32 = maybe_value(inline209)
        var inline212 int32
        switch inline210.(type) {
        case None:
            t183 = None{}
            var t184 string
            switch t183.(type) {
            case None:
                t184 = "none"
            case Some:
                var inline204 int32 = t183.(Some)._0
                var inline206 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline204)
                var inline207 string = "some=" + inline206
                t184 = inline207
            default:
                panic("non-exhaustive match")
            }
            var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
            _goml_runtime_core_string_println(inline201)
            return struct{}{}
        case Some:
            var inline215 int32 = inline210.(Some)._0
            inline212 = inline215
            var inline213 int32 = add(inline212, 2)
            var inline214 Option__int32 = Some{
                _0: inline213,
            }
            t183 = inline214
            var t184 string
            switch t183.(type) {
            case None:
                t184 = "none"
            case Some:
                var inline204 int32 = t183.(Some)._0
                var inline206 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline204)
                var inline207 string = "some=" + inline206
                t184 = inline207
            default:
                panic("non-exhaustive match")
            }
            var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
            _goml_runtime_core_string_println(inline201)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t187 string = _goml_runtime_core_int32_to_string(self__6)
    return t187
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
