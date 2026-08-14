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

type Result__int32__string interface {
    isResult__int32__string()
}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

func parse(flag__0 bool) Result__int32__string {
    if flag__0 {
        var t197 Result__int32__string = Ok{
            _0: 41,
        }
        return t197
    } else {
        var t198 Result__int32__string = Err{
            _0: "bad",
        }
        return t198
    }
}

func compute(flag__1 bool) Result__int32__string {
    var mtmp187 Result__int32__string
    if flag__1 {
        var inline226 Result__int32__string = Ok{
            _0: 41,
        }
        mtmp187 = inline226
    } else {
        var inline227 Result__int32__string = Err{
            _0: "bad",
        }
        mtmp187 = inline227
    }
    var jp202 int32
    switch mtmp187.(type) {
    case Err:
        var x188 string = mtmp187.(Err)._0
        var t205 Result__int32__string = Err{
            _0: x188,
        }
        return t205
    case Ok:
        var x189 int32 = mtmp187.(Ok)._0
        jp202 = x189
        var t203 int32 = jp202 + 1
        var t204 Result__int32__string = Ok{
            _0: t203,
        }
        return t204
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t212 Result__int32__string = compute(true)
    var t213 string
    switch t212.(type) {
    case Err:
        var inline256 string = t212.(Err)._0
        t213 = inline256
    case Ok:
        var inline258 int32 = t212.(Ok)._0
        var inline260 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline258)
        t213 = inline260
    default:
        panic("non-exhaustive match")
    }
    var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline253)
    var t214 Result__int32__string
    var inline240 bool = false
    var inline241 Result__int32__string = parse(inline240)
    var inline243 int32
    switch inline241.(type) {
    case Err:
        var inline247 string = inline241.(Err)._0
        var inline249 Result__int32__string = Err{
            _0: inline247,
        }
        t214 = inline249
        var t215 string
        switch t214.(type) {
        case Err:
            var inline234 string = t214.(Err)._0
            t215 = inline234
        case Ok:
            var inline236 int32 = t214.(Ok)._0
            var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
            t215 = inline238
        default:
            panic("non-exhaustive match")
        }
        var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
        _goml_runtime_core_string_println(inline231)
        return struct{}{}
    case Ok:
        var inline250 int32 = inline241.(Ok)._0
        inline243 = inline250
        var inline245 int32 = inline243 + 1
        var inline246 Result__int32__string = Ok{
            _0: inline245,
        }
        t214 = inline246
        var t215 string
        switch t214.(type) {
        case Err:
            var inline234 string = t214.(Err)._0
            t215 = inline234
        case Ok:
            var inline236 int32 = t214.(Ok)._0
            var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
            t215 = inline238
        default:
            panic("non-exhaustive match")
        }
        var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
        _goml_runtime_core_string_println(inline231)
        return struct{}{}
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
