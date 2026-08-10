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
        var t181 Option__int32 = Some{
            _0: 4,
        }
        return t181
    } else {
        return None{}
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var t184 int32 = a__1 + b__2
    return t184
}

func main0() struct{} {
    var t198 Option__int32
    var inline243 bool = true
    var inline244 Option__int32 = maybe_value(inline243)
    var inline246 int32
    switch inline244.(type) {
    case None:
        t198 = None{}
        var t199 string
        switch t198.(type) {
        case None:
            t199 = "none"
        case Some:
            var inline238 int32 = t198.(Some)._0
            var inline240 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline238)
            var inline241 string = "some=" + inline240
            t199 = inline241
        default:
            panic("non-exhaustive match")
        }
        var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
        _goml_runtime_core_string_println(inline235)
        var t200 Option__int32
        var inline226 bool = false
        var inline227 Option__int32 = maybe_value(inline226)
        var inline229 int32
        switch inline227.(type) {
        case None:
            t200 = None{}
            var t201 string
            switch t200.(type) {
            case None:
                t201 = "none"
            case Some:
                var inline221 int32 = t200.(Some)._0
                var inline223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline221)
                var inline224 string = "some=" + inline223
                t201 = inline224
            default:
                panic("non-exhaustive match")
            }
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
            _goml_runtime_core_string_println(inline218)
            return struct{}{}
        case Some:
            var inline232 int32 = inline227.(Some)._0
            inline229 = inline232
            var inline230 int32 = add(inline229, 2)
            var inline231 Option__int32 = Some{
                _0: inline230,
            }
            t200 = inline231
            var t201 string
            switch t200.(type) {
            case None:
                t201 = "none"
            case Some:
                var inline221 int32 = t200.(Some)._0
                var inline223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline221)
                var inline224 string = "some=" + inline223
                t201 = inline224
            default:
                panic("non-exhaustive match")
            }
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
            _goml_runtime_core_string_println(inline218)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline249 int32 = inline244.(Some)._0
        inline246 = inline249
        var inline247 int32 = add(inline246, 2)
        var inline248 Option__int32 = Some{
            _0: inline247,
        }
        t198 = inline248
        var t199 string
        switch t198.(type) {
        case None:
            t199 = "none"
        case Some:
            var inline238 int32 = t198.(Some)._0
            var inline240 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline238)
            var inline241 string = "some=" + inline240
            t199 = inline241
        default:
            panic("non-exhaustive match")
        }
        var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
        _goml_runtime_core_string_println(inline235)
        var t200 Option__int32
        var inline226 bool = false
        var inline227 Option__int32 = maybe_value(inline226)
        var inline229 int32
        switch inline227.(type) {
        case None:
            t200 = None{}
            var t201 string
            switch t200.(type) {
            case None:
                t201 = "none"
            case Some:
                var inline221 int32 = t200.(Some)._0
                var inline223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline221)
                var inline224 string = "some=" + inline223
                t201 = inline224
            default:
                panic("non-exhaustive match")
            }
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
            _goml_runtime_core_string_println(inline218)
            return struct{}{}
        case Some:
            var inline232 int32 = inline227.(Some)._0
            inline229 = inline232
            var inline230 int32 = add(inline229, 2)
            var inline231 Option__int32 = Some{
                _0: inline230,
            }
            t200 = inline231
            var t201 string
            switch t200.(type) {
            case None:
                t201 = "none"
            case Some:
                var inline221 int32 = t200.(Some)._0
                var inline223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline221)
                var inline224 string = "some=" + inline223
                t201 = inline224
            default:
                panic("non-exhaustive match")
            }
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
            _goml_runtime_core_string_println(inline218)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t204 string = _goml_runtime_core_int32_to_string(self__33)
    return t204
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
