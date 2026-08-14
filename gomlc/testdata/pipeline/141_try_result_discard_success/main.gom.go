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
        var t194 Result__string__string = Ok{
            _0: "ignored",
        }
        return t194
    } else {
        var t195 Result__string__string = Err{
            _0: "parse failed",
        }
        return t195
    }
}

func check(ok__1 bool) Result__string__string {
    var mtmp182 Result__string__string
    if ok__1 {
        var inline219 Result__string__string = Ok{
            _0: "ignored",
        }
        mtmp182 = inline219
    } else {
        var inline220 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp182 = inline220
    }
    switch mtmp182.(type) {
    case Ok:
        var t200 Result__string__string = Ok{
            _0: "ok",
        }
        return t200
    case Err:
        var x184 string = mtmp182.(Err)._0
        var t201 Result__string__string = Err{
            _0: x184,
        }
        return t201
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t209 Result__string__string = check(true)
    var t210 string
    switch t209.(type) {
    case Ok:
        var inline247 string = t209.(Ok)._0
        var inline249 string = "ok " + inline247
        t210 = inline249
    case Err:
        var inline250 string = t209.(Err)._0
        var inline252 string = "err " + inline250
        t210 = inline252
    default:
        panic("non-exhaustive match")
    }
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline244)
    var t211 Result__string__string
    var inline232 bool = false
    var inline233 Result__string__string = parse_text(inline232)
    switch inline233.(type) {
    case Ok:
        var inline237 Result__string__string = Ok{
            _0: "ok",
        }
        t211 = inline237
        var t212 string
        switch t211.(type) {
        case Ok:
            var inline225 string = t211.(Ok)._0
            var inline227 string = "ok " + inline225
            t212 = inline227
        case Err:
            var inline228 string = t211.(Err)._0
            var inline230 string = "err " + inline228
            t212 = inline230
        default:
            panic("non-exhaustive match")
        }
        var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
        _goml_runtime_core_string_println(inline222)
        return struct{}{}
    case Err:
        var inline240 string = inline233.(Err)._0
        var inline242 Result__string__string = Err{
            _0: inline240,
        }
        t211 = inline242
        var t212 string
        switch t211.(type) {
        case Ok:
            var inline225 string = t211.(Ok)._0
            var inline227 string = "ok " + inline225
            t212 = inline227
        case Err:
            var inline228 string = t211.(Err)._0
            var inline230 string = "err " + inline228
            t212 = inline230
        default:
            panic("non-exhaustive match")
        }
        var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
        _goml_runtime_core_string_println(inline222)
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
