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
        var t190 Option__int32 = Some{
            _0: 41,
        }
        return t190
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t203 Option__int32
    var inline245 bool = true
    var inline246 Option__int32 = maybe_value(inline245)
    var inline248 int32
    switch inline246.(type) {
    case Some:
        var inline252 int32 = inline246.(Some)._0
        inline248 = inline252
        var inline250 int32 = inline248 + 1
        var inline251 Option__int32 = Some{
            _0: inline250,
        }
        t203 = inline251
        var t204 string
        switch t203.(type) {
        case Some:
            var inline241 int32 = t203.(Some)._0
            var inline243 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline241)
            t204 = inline243
        case None:
            t204 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
        _goml_runtime_core_string_println(inline238)
        var t205 Option__int32
        var inline228 bool = false
        var inline229 Option__int32 = maybe_value(inline228)
        var inline231 int32
        switch inline229.(type) {
        case Some:
            var inline235 int32 = inline229.(Some)._0
            inline231 = inline235
            var inline233 int32 = inline231 + 1
            var inline234 Option__int32 = Some{
                _0: inline233,
            }
            t205 = inline234
            var t206 string
            switch t205.(type) {
            case Some:
                var inline224 int32 = t205.(Some)._0
                var inline226 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline224)
                t206 = inline226
            case None:
                t206 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline221)
            return struct{}{}
        case None:
            t205 = None{}
            var t206 string
            switch t205.(type) {
            case Some:
                var inline224 int32 = t205.(Some)._0
                var inline226 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline224)
                t206 = inline226
            case None:
                t206 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline221)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case None:
        t203 = None{}
        var t204 string
        switch t203.(type) {
        case Some:
            var inline241 int32 = t203.(Some)._0
            var inline243 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline241)
            t204 = inline243
        case None:
            t204 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
        _goml_runtime_core_string_println(inline238)
        var t205 Option__int32
        var inline228 bool = false
        var inline229 Option__int32 = maybe_value(inline228)
        var inline231 int32
        switch inline229.(type) {
        case Some:
            var inline235 int32 = inline229.(Some)._0
            inline231 = inline235
            var inline233 int32 = inline231 + 1
            var inline234 Option__int32 = Some{
                _0: inline233,
            }
            t205 = inline234
            var t206 string
            switch t205.(type) {
            case Some:
                var inline224 int32 = t205.(Some)._0
                var inline226 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline224)
                t206 = inline226
            case None:
                t206 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline221)
            return struct{}{}
        case None:
            t205 = None{}
            var t206 string
            switch t205.(type) {
            case Some:
                var inline224 int32 = t205.(Some)._0
                var inline226 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline224)
                t206 = inline226
            case None:
                t206 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline221)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t210 string = _goml_runtime_core_int32_to_string(self__33)
    return t210
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
