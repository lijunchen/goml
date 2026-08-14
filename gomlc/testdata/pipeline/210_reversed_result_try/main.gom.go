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
        var t192 Result__int32__string = Ok{
            _0: 41,
        }
        return t192
    } else {
        var t193 Result__int32__string = Err{
            _0: "bad",
        }
        return t193
    }
}

func compute(flag__1 bool) Result__int32__string {
    var mtmp182 Result__int32__string
    if flag__1 {
        var inline221 Result__int32__string = Ok{
            _0: 41,
        }
        mtmp182 = inline221
    } else {
        var inline222 Result__int32__string = Err{
            _0: "bad",
        }
        mtmp182 = inline222
    }
    var jp197 int32
    switch mtmp182.(type) {
    case Err:
        var x183 string = mtmp182.(Err)._0
        var t200 Result__int32__string = Err{
            _0: x183,
        }
        return t200
    case Ok:
        var x184 int32 = mtmp182.(Ok)._0
        jp197 = x184
        var t198 int32 = jp197 + 1
        var t199 Result__int32__string = Ok{
            _0: t198,
        }
        return t199
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t207 Result__int32__string = compute(true)
    var t208 string
    switch t207.(type) {
    case Err:
        var inline251 string = t207.(Err)._0
        t208 = inline251
    case Ok:
        var inline253 int32 = t207.(Ok)._0
        var inline255 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline253)
        t208 = inline255
    default:
        panic("non-exhaustive match")
    }
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline248)
    var t209 Result__int32__string
    var inline235 bool = false
    var inline236 Result__int32__string = parse(inline235)
    var inline238 int32
    switch inline236.(type) {
    case Err:
        var inline242 string = inline236.(Err)._0
        var inline244 Result__int32__string = Err{
            _0: inline242,
        }
        t209 = inline244
        var t210 string
        switch t209.(type) {
        case Err:
            var inline229 string = t209.(Err)._0
            t210 = inline229
        case Ok:
            var inline231 int32 = t209.(Ok)._0
            var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
            t210 = inline233
        default:
            panic("non-exhaustive match")
        }
        var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
        _goml_runtime_core_string_println(inline226)
        return struct{}{}
    case Ok:
        var inline245 int32 = inline236.(Ok)._0
        inline238 = inline245
        var inline240 int32 = inline238 + 1
        var inline241 Result__int32__string = Ok{
            _0: inline240,
        }
        t209 = inline241
        var t210 string
        switch t209.(type) {
        case Err:
            var inline229 string = t209.(Err)._0
            t210 = inline229
        case Ok:
            var inline231 int32 = t209.(Ok)._0
            var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline231)
            t210 = inline233
        default:
            panic("non-exhaustive match")
        }
        var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
        _goml_runtime_core_string_println(inline226)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t214 string = _goml_runtime_core_int32_to_string(self__33)
    return t214
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
