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
        var t189 Result__string__string = Ok{
            _0: "ignored",
        }
        return t189
    } else {
        var t190 Result__string__string = Err{
            _0: "parse failed",
        }
        return t190
    }
}

func check(ok__1 bool) Result__string__string {
    var mtmp177 Result__string__string
    if ok__1 {
        var inline214 Result__string__string = Ok{
            _0: "ignored",
        }
        mtmp177 = inline214
    } else {
        var inline215 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp177 = inline215
    }
    switch mtmp177.(type) {
    case Ok:
        var t195 Result__string__string = Ok{
            _0: "ok",
        }
        return t195
    case Err:
        var x179 string = mtmp177.(Err)._0
        var t196 Result__string__string = Err{
            _0: x179,
        }
        return t196
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t204 Result__string__string = check(true)
    var t205 string
    switch t204.(type) {
    case Ok:
        var inline242 string = t204.(Ok)._0
        var inline244 string = "ok " + inline242
        t205 = inline244
    case Err:
        var inline245 string = t204.(Err)._0
        var inline247 string = "err " + inline245
        t205 = inline247
    default:
        panic("non-exhaustive match")
    }
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline239)
    var t206 Result__string__string
    var inline227 bool = false
    var inline228 Result__string__string = parse_text(inline227)
    switch inline228.(type) {
    case Ok:
        var inline232 Result__string__string = Ok{
            _0: "ok",
        }
        t206 = inline232
        var t207 string
        switch t206.(type) {
        case Ok:
            var inline220 string = t206.(Ok)._0
            var inline222 string = "ok " + inline220
            t207 = inline222
        case Err:
            var inline223 string = t206.(Err)._0
            var inline225 string = "err " + inline223
            t207 = inline225
        default:
            panic("non-exhaustive match")
        }
        var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
        _goml_runtime_core_string_println(inline217)
        return struct{}{}
    case Err:
        var inline235 string = inline228.(Err)._0
        var inline237 Result__string__string = Err{
            _0: inline235,
        }
        t206 = inline237
        var t207 string
        switch t206.(type) {
        case Ok:
            var inline220 string = t206.(Ok)._0
            var inline222 string = "ok " + inline220
            t207 = inline222
        case Err:
            var inline223 string = t206.(Err)._0
            var inline225 string = "err " + inline223
            t207 = inline225
        default:
            panic("non-exhaustive match")
        }
        var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
        _goml_runtime_core_string_println(inline217)
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
