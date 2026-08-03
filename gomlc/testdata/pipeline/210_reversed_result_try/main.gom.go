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
        var t187 Result__int32__string = Ok{
            _0: 41,
        }
        return t187
    } else {
        var t188 Result__int32__string = Err{
            _0: "bad",
        }
        return t188
    }
}

func compute(flag__1 bool) Result__int32__string {
    var mtmp177 Result__int32__string
    if flag__1 {
        var inline216 Result__int32__string = Ok{
            _0: 41,
        }
        mtmp177 = inline216
    } else {
        var inline217 Result__int32__string = Err{
            _0: "bad",
        }
        mtmp177 = inline217
    }
    var jp192 int32
    switch mtmp177.(type) {
    case Err:
        var x178 string = mtmp177.(Err)._0
        var t195 Result__int32__string = Err{
            _0: x178,
        }
        return t195
    case Ok:
        var x179 int32 = mtmp177.(Ok)._0
        jp192 = x179
        var t193 int32 = jp192 + 1
        var t194 Result__int32__string = Ok{
            _0: t193,
        }
        return t194
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t202 Result__int32__string = compute(true)
    var t203 string
    switch t202.(type) {
    case Err:
        var inline246 string = t202.(Err)._0
        t203 = inline246
    case Ok:
        var inline248 int32 = t202.(Ok)._0
        var inline250 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline248)
        t203 = inline250
    default:
        panic("non-exhaustive match")
    }
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline243)
    var t204 Result__int32__string
    var inline230 bool = false
    var inline231 Result__int32__string = parse(inline230)
    var inline233 int32
    switch inline231.(type) {
    case Err:
        var inline237 string = inline231.(Err)._0
        var inline239 Result__int32__string = Err{
            _0: inline237,
        }
        t204 = inline239
        var t205 string
        switch t204.(type) {
        case Err:
            var inline224 string = t204.(Err)._0
            t205 = inline224
        case Ok:
            var inline226 int32 = t204.(Ok)._0
            var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
            t205 = inline228
        default:
            panic("non-exhaustive match")
        }
        var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
        _goml_runtime_core_string_println(inline221)
        return struct{}{}
    case Ok:
        var inline240 int32 = inline231.(Ok)._0
        inline233 = inline240
        var inline235 int32 = inline233 + 1
        var inline236 Result__int32__string = Ok{
            _0: inline235,
        }
        t204 = inline236
        var t205 string
        switch t204.(type) {
        case Err:
            var inline224 string = t204.(Err)._0
            t205 = inline224
        case Ok:
            var inline226 int32 = t204.(Ok)._0
            var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
            t205 = inline228
        default:
            panic("non-exhaustive match")
        }
        var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
        _goml_runtime_core_string_println(inline221)
        return struct{}{}
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
