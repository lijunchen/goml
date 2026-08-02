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
        var t165 Result__int32__string = Ok{
            _0: 41,
        }
        return t165
    } else {
        var t166 Result__int32__string = Err{
            _0: "bad",
        }
        return t166
    }
}

func compute(flag__1 bool) Result__int32__string {
    var mtmp155 Result__int32__string
    if flag__1 {
        var inline194 Result__int32__string = Ok{
            _0: 41,
        }
        mtmp155 = inline194
    } else {
        var inline195 Result__int32__string = Err{
            _0: "bad",
        }
        mtmp155 = inline195
    }
    var jp170 int32
    switch mtmp155.(type) {
    case Err:
        var x156 string = mtmp155.(Err)._0
        var t173 Result__int32__string = Err{
            _0: x156,
        }
        return t173
    case Ok:
        var x157 int32 = mtmp155.(Ok)._0
        jp170 = x157
        var t171 int32 = jp170 + 1
        var t172 Result__int32__string = Ok{
            _0: t171,
        }
        return t172
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t180 Result__int32__string = compute(true)
    var t181 string
    switch t180.(type) {
    case Err:
        var inline224 string = t180.(Err)._0
        t181 = inline224
    case Ok:
        var inline226 int32 = t180.(Ok)._0
        var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
        t181 = inline228
    default:
        panic("non-exhaustive match")
    }
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline221)
    var t182 Result__int32__string
    var inline208 bool = false
    var inline209 Result__int32__string = parse(inline208)
    var inline211 int32
    switch inline209.(type) {
    case Err:
        var inline215 string = inline209.(Err)._0
        var inline217 Result__int32__string = Err{
            _0: inline215,
        }
        t182 = inline217
        var t183 string
        switch t182.(type) {
        case Err:
            var inline202 string = t182.(Err)._0
            t183 = inline202
        case Ok:
            var inline204 int32 = t182.(Ok)._0
            var inline206 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline204)
            t183 = inline206
        default:
            panic("non-exhaustive match")
        }
        var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
        _goml_runtime_core_string_println(inline199)
        return struct{}{}
    case Ok:
        var inline218 int32 = inline209.(Ok)._0
        inline211 = inline218
        var inline213 int32 = inline211 + 1
        var inline214 Result__int32__string = Ok{
            _0: inline213,
        }
        t182 = inline214
        var t183 string
        switch t182.(type) {
        case Err:
            var inline202 string = t182.(Err)._0
            t183 = inline202
        case Ok:
            var inline204 int32 = t182.(Ok)._0
            var inline206 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline204)
            t183 = inline206
        default:
            panic("non-exhaustive match")
        }
        var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
        _goml_runtime_core_string_println(inline199)
        return struct{}{}
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
