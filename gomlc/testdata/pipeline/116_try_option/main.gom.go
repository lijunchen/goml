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
        var t196 Option__int32 = Some{
            _0: 4,
        }
        return t196
    } else {
        return None{}
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var t199 int32 = a__1 + b__2
    return t199
}

func main0() struct{} {
    var t213 Option__int32
    var inline258 bool = true
    var inline259 Option__int32 = maybe_value(inline258)
    var inline261 int32
    switch inline259.(type) {
    case None:
        t213 = None{}
        var t214 string
        switch t213.(type) {
        case None:
            t214 = "none"
        case Some:
            var inline253 int32 = t213.(Some)._0
            var inline255 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline253)
            var inline256 string = "some=" + inline255
            t214 = inline256
        default:
            panic("non-exhaustive match")
        }
        var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
        _goml_runtime_core_string_println(inline250)
        var t215 Option__int32
        var inline241 bool = false
        var inline242 Option__int32 = maybe_value(inline241)
        var inline244 int32
        switch inline242.(type) {
        case None:
            t215 = None{}
            var t216 string
            switch t215.(type) {
            case None:
                t216 = "none"
            case Some:
                var inline236 int32 = t215.(Some)._0
                var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
                var inline239 string = "some=" + inline238
                t216 = inline239
            default:
                panic("non-exhaustive match")
            }
            var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
            _goml_runtime_core_string_println(inline233)
            return struct{}{}
        case Some:
            var inline247 int32 = inline242.(Some)._0
            inline244 = inline247
            var inline245 int32 = add(inline244, 2)
            var inline246 Option__int32 = Some{
                _0: inline245,
            }
            t215 = inline246
            var t216 string
            switch t215.(type) {
            case None:
                t216 = "none"
            case Some:
                var inline236 int32 = t215.(Some)._0
                var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
                var inline239 string = "some=" + inline238
                t216 = inline239
            default:
                panic("non-exhaustive match")
            }
            var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
            _goml_runtime_core_string_println(inline233)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case Some:
        var inline264 int32 = inline259.(Some)._0
        inline261 = inline264
        var inline262 int32 = add(inline261, 2)
        var inline263 Option__int32 = Some{
            _0: inline262,
        }
        t213 = inline263
        var t214 string
        switch t213.(type) {
        case None:
            t214 = "none"
        case Some:
            var inline253 int32 = t213.(Some)._0
            var inline255 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline253)
            var inline256 string = "some=" + inline255
            t214 = inline256
        default:
            panic("non-exhaustive match")
        }
        var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
        _goml_runtime_core_string_println(inline250)
        var t215 Option__int32
        var inline241 bool = false
        var inline242 Option__int32 = maybe_value(inline241)
        var inline244 int32
        switch inline242.(type) {
        case None:
            t215 = None{}
            var t216 string
            switch t215.(type) {
            case None:
                t216 = "none"
            case Some:
                var inline236 int32 = t215.(Some)._0
                var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
                var inline239 string = "some=" + inline238
                t216 = inline239
            default:
                panic("non-exhaustive match")
            }
            var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
            _goml_runtime_core_string_println(inline233)
            return struct{}{}
        case Some:
            var inline247 int32 = inline242.(Some)._0
            inline244 = inline247
            var inline245 int32 = add(inline244, 2)
            var inline246 Option__int32 = Some{
                _0: inline245,
            }
            t215 = inline246
            var t216 string
            switch t215.(type) {
            case None:
                t216 = "none"
            case Some:
                var inline236 int32 = t215.(Some)._0
                var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
                var inline239 string = "some=" + inline238
                t216 = inline239
            default:
                panic("non-exhaustive match")
            }
            var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
            _goml_runtime_core_string_println(inline233)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t219 string = _goml_runtime_core_int32_to_string(self__33)
    return t219
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
