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
        var t199 Result__string__string = Ok{
            _0: "ignored",
        }
        return t199
    } else {
        var t200 Result__string__string = Err{
            _0: "parse failed",
        }
        return t200
    }
}

func check(ok__1 bool) Result__string__string {
    var mtmp187 Result__string__string
    if ok__1 {
        var inline224 Result__string__string = Ok{
            _0: "ignored",
        }
        mtmp187 = inline224
    } else {
        var inline225 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp187 = inline225
    }
    switch mtmp187.(type) {
    case Ok:
        var t205 Result__string__string = Ok{
            _0: "ok",
        }
        return t205
    case Err:
        var x189 string = mtmp187.(Err)._0
        var t206 Result__string__string = Err{
            _0: x189,
        }
        return t206
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t214 Result__string__string = check(true)
    var t215 string
    switch t214.(type) {
    case Ok:
        var inline252 string = t214.(Ok)._0
        var inline254 string = "ok " + inline252
        t215 = inline254
    case Err:
        var inline255 string = t214.(Err)._0
        var inline257 string = "err " + inline255
        t215 = inline257
    default:
        panic("non-exhaustive match")
    }
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline249)
    var t216 Result__string__string
    var inline237 bool = false
    var inline238 Result__string__string = parse_text(inline237)
    switch inline238.(type) {
    case Ok:
        var inline242 Result__string__string = Ok{
            _0: "ok",
        }
        t216 = inline242
        var t217 string
        switch t216.(type) {
        case Ok:
            var inline230 string = t216.(Ok)._0
            var inline232 string = "ok " + inline230
            t217 = inline232
        case Err:
            var inline233 string = t216.(Err)._0
            var inline235 string = "err " + inline233
            t217 = inline235
        default:
            panic("non-exhaustive match")
        }
        var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
        _goml_runtime_core_string_println(inline227)
        return struct{}{}
    case Err:
        var inline245 string = inline238.(Err)._0
        var inline247 Result__string__string = Err{
            _0: inline245,
        }
        t216 = inline247
        var t217 string
        switch t216.(type) {
        case Ok:
            var inline230 string = t216.(Ok)._0
            var inline232 string = "ok " + inline230
            t217 = inline232
        case Err:
            var inline233 string = t216.(Err)._0
            var inline235 string = "err " + inline233
            t217 = inline235
        default:
            panic("non-exhaustive match")
        }
        var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
        _goml_runtime_core_string_println(inline227)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
