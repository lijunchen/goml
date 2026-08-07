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
        var t182 Result__int32__string = Ok{
            _0: 41,
        }
        return t182
    } else {
        var t183 Result__int32__string = Err{
            _0: "bad",
        }
        return t183
    }
}

func compute(flag__1 bool) Result__int32__string {
    var mtmp172 Result__int32__string
    if flag__1 {
        var inline211 Result__int32__string = Ok{
            _0: 41,
        }
        mtmp172 = inline211
    } else {
        var inline212 Result__int32__string = Err{
            _0: "bad",
        }
        mtmp172 = inline212
    }
    var jp187 int32
    switch mtmp172.(type) {
    case Err:
        var x173 string = mtmp172.(Err)._0
        var t190 Result__int32__string = Err{
            _0: x173,
        }
        return t190
    case Ok:
        var x174 int32 = mtmp172.(Ok)._0
        jp187 = x174
        var t188 int32 = jp187 + 1
        var t189 Result__int32__string = Ok{
            _0: t188,
        }
        return t189
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t197 Result__int32__string = compute(true)
    var t198 string
    switch t197.(type) {
    case Err:
        var inline241 string = t197.(Err)._0
        t198 = inline241
    case Ok:
        var inline243 int32 = t197.(Ok)._0
        var inline245 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline243)
        t198 = inline245
    default:
        panic("non-exhaustive match")
    }
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline238)
    var t199 Result__int32__string
    var inline225 bool = false
    var inline226 Result__int32__string = parse(inline225)
    var inline228 int32
    switch inline226.(type) {
    case Err:
        var inline232 string = inline226.(Err)._0
        var inline234 Result__int32__string = Err{
            _0: inline232,
        }
        t199 = inline234
        var t200 string
        switch t199.(type) {
        case Err:
            var inline219 string = t199.(Err)._0
            t200 = inline219
        case Ok:
            var inline221 int32 = t199.(Ok)._0
            var inline223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline221)
            t200 = inline223
        default:
            panic("non-exhaustive match")
        }
        var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
        _goml_runtime_core_string_println(inline216)
        return struct{}{}
    case Ok:
        var inline235 int32 = inline226.(Ok)._0
        inline228 = inline235
        var inline230 int32 = inline228 + 1
        var inline231 Result__int32__string = Ok{
            _0: inline230,
        }
        t199 = inline231
        var t200 string
        switch t199.(type) {
        case Err:
            var inline219 string = t199.(Err)._0
            t200 = inline219
        case Ok:
            var inline221 int32 = t199.(Ok)._0
            var inline223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline221)
            t200 = inline223
        default:
            panic("non-exhaustive match")
        }
        var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
        _goml_runtime_core_string_println(inline216)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t204 string = _goml_runtime_core_int32_to_string(self__35)
    return t204
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
