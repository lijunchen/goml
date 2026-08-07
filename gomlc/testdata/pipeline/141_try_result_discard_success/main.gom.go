package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Result__string__string interface {
    isResult__string__string()
}

type Ok struct {
    _0 string
}

func (_ Ok) isResult__string__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__string__string() {}

func parse_text(ok__0 bool) Result__string__string {
    if ok__0 {
        var t184 Result__string__string = Ok{
            _0: "ignored",
        }
        return t184
    } else {
        var t185 Result__string__string = Err{
            _0: "parse failed",
        }
        return t185
    }
}

func check(ok__1 bool) Result__string__string {
    var mtmp172 Result__string__string
    if ok__1 {
        var inline209 Result__string__string = Ok{
            _0: "ignored",
        }
        mtmp172 = inline209
    } else {
        var inline210 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp172 = inline210
    }
    switch mtmp172.(type) {
    case Ok:
        var t190 Result__string__string = Ok{
            _0: "ok",
        }
        return t190
    case Err:
        var x174 string = mtmp172.(Err)._0
        var t191 Result__string__string = Err{
            _0: x174,
        }
        return t191
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t199 Result__string__string = check(true)
    var t200 string
    switch t199.(type) {
    case Ok:
        var inline237 string = t199.(Ok)._0
        var inline239 string = "ok " + inline237
        t200 = inline239
    case Err:
        var inline240 string = t199.(Err)._0
        var inline242 string = "err " + inline240
        t200 = inline242
    default:
        panic("non-exhaustive match")
    }
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline234)
    var t201 Result__string__string
    var inline222 bool = false
    var inline223 Result__string__string = parse_text(inline222)
    switch inline223.(type) {
    case Ok:
        var inline227 Result__string__string = Ok{
            _0: "ok",
        }
        t201 = inline227
        var t202 string
        switch t201.(type) {
        case Ok:
            var inline215 string = t201.(Ok)._0
            var inline217 string = "ok " + inline215
            t202 = inline217
        case Err:
            var inline218 string = t201.(Err)._0
            var inline220 string = "err " + inline218
            t202 = inline220
        default:
            panic("non-exhaustive match")
        }
        var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
        _goml_runtime_core_string_println(inline212)
        return struct{}{}
    case Err:
        var inline230 string = inline223.(Err)._0
        var inline232 Result__string__string = Err{
            _0: inline230,
        }
        t201 = inline232
        var t202 string
        switch t201.(type) {
        case Ok:
            var inline215 string = t201.(Ok)._0
            var inline217 string = "ok " + inline215
            t202 = inline217
        case Err:
            var inline218 string = t201.(Err)._0
            var inline220 string = "err " + inline218
            t202 = inline220
        default:
            panic("non-exhaustive match")
        }
        var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
        _goml_runtime_core_string_println(inline212)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
