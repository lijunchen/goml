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
        var t186 Option__int32 = Some{
            _0: 4,
        }
        return t186
    } else {
        return None{}
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var t189 int32 = a__1 + b__2
    return t189
}

func main0() struct{} {
    var t203 Option__int32
    var inline248 bool = true
    var inline249 Option__int32 = maybe_value(inline248)
    var inline251 int32
    switch inline249.(type) {
    case None:
        t203 = None{}
        var t204 string
        switch t203.(type) {
        case None:
            t204 = "none"
        case Some:
            var inline243 int32 = t203.(Some)._0
            var inline245 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline243)
            var inline246 string = "some=" + inline245
            t204 = inline246
        default:
            panic("non-exhaustive match")
        }
        var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
        _goml_runtime_core_string_println(inline240)
        var t205 Option__int32
        var inline231 bool = false
        var inline232 Option__int32 = maybe_value(inline231)
        var inline234 int32
        switch inline232.(type) {
        case None:
            t205 = None{}
            var t206 string
            switch t205.(type) {
            case None:
                t206 = "none"
            case Some:
                var inline226 int32 = t205.(Some)._0
                var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
                var inline229 string = "some=" + inline228
                t206 = inline229
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline223)
            return struct{}{}
        case Some:
            var inline237 int32 = inline232.(Some)._0
            inline234 = inline237
            var inline235 int32 = add(inline234, 2)
            var inline236 Option__int32 = Some{
                _0: inline235,
            }
            t205 = inline236
            var t206 string
            switch t205.(type) {
            case None:
                t206 = "none"
            case Some:
                var inline226 int32 = t205.(Some)._0
                var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
                var inline229 string = "some=" + inline228
                t206 = inline229
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline223)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline254 int32 = inline249.(Some)._0
        inline251 = inline254
        var inline252 int32 = add(inline251, 2)
        var inline253 Option__int32 = Some{
            _0: inline252,
        }
        t203 = inline253
        var t204 string
        switch t203.(type) {
        case None:
            t204 = "none"
        case Some:
            var inline243 int32 = t203.(Some)._0
            var inline245 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline243)
            var inline246 string = "some=" + inline245
            t204 = inline246
        default:
            panic("non-exhaustive match")
        }
        var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
        _goml_runtime_core_string_println(inline240)
        var t205 Option__int32
        var inline231 bool = false
        var inline232 Option__int32 = maybe_value(inline231)
        var inline234 int32
        switch inline232.(type) {
        case None:
            t205 = None{}
            var t206 string
            switch t205.(type) {
            case None:
                t206 = "none"
            case Some:
                var inline226 int32 = t205.(Some)._0
                var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
                var inline229 string = "some=" + inline228
                t206 = inline229
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline223)
            return struct{}{}
        case Some:
            var inline237 int32 = inline232.(Some)._0
            inline234 = inline237
            var inline235 int32 = add(inline234, 2)
            var inline236 Option__int32 = Some{
                _0: inline235,
            }
            t205 = inline236
            var t206 string
            switch t205.(type) {
            case None:
                t206 = "none"
            case Some:
                var inline226 int32 = t205.(Some)._0
                var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
                var inline229 string = "some=" + inline228
                t206 = inline229
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline223)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t209 string = _goml_runtime_core_int32_to_string(self__35)
    return t209
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
