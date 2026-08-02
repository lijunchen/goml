package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Handle struct {
    name string
}

type Result__Handle__string interface {
    isResult__Handle__string()
}

type Result__Handle__string_Ok struct {
    _0 Handle
}

func (_ Result__Handle__string_Ok) isResult__Handle__string() {}

type Result__Handle__string_Err struct {
    _0 string
}

func (_ Result__Handle__string_Err) isResult__Handle__string() {}

type Result__unit__string interface {
    isResult__unit__string()
}

type Result__unit__string_Ok struct {
    _0 struct{}
}

func (_ Result__unit__string_Ok) isResult__unit__string() {}

type Result__unit__string_Err struct {
    _0 string
}

func (_ Result__unit__string_Err) isResult__unit__string() {}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var mtmp155 Result__Handle__string
    if open_ok__3 {
        var inline215 Handle = Handle{
            name: "config",
        }
        var inline216 Result__Handle__string = Result__Handle__string_Ok{
            _0: inline215,
        }
        mtmp155 = inline216
    } else {
        var inline217 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        mtmp155 = inline217
    }
    var jp185 Handle
    switch mtmp155.(type) {
    case Result__Handle__string_Ok:
        var x156 Handle = mtmp155.(Result__Handle__string_Ok)._0
        jp185 = x156
        var name__6 string = jp185.name
        var mtmp158 Result__unit__string
        if close_ok__4 {
            var inline210 Result__unit__string = Result__unit__string_Ok{
                _0: struct{}{},
            }
            mtmp158 = inline210
        } else {
            var inline211 string = jp185.name
            var inline212 string = "close failed for " + inline211
            var inline213 Result__unit__string = Result__unit__string_Err{
                _0: inline212,
            }
            mtmp158 = inline213
        }
        switch mtmp158.(type) {
        case Result__unit__string_Ok:
            var t187 string = "closed " + name__6
            var t188 Result__string__string = Result__string__string_Ok{
                _0: t187,
            }
            return t188
        case Result__unit__string_Err:
            var x160 string = mtmp158.(Result__unit__string_Err)._0
            var t189 Result__string__string = Result__string__string_Err{
                _0: x160,
            }
            return t189
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x157 string = mtmp155.(Result__Handle__string_Err)._0
        var t190 Result__string__string = Result__string__string_Err{
            _0: x157,
        }
        return t190
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t198 Result__string__string = use_handle(true, true)
    var t199 string
    switch t198.(type) {
    case Result__string__string_Ok:
        var inline242 string = t198.(Result__string__string_Ok)._0
        var inline244 string = "ok " + inline242
        t199 = inline244
    case Result__string__string_Err:
        var inline245 string = t198.(Result__string__string_Err)._0
        var inline247 string = "err " + inline245
        t199 = inline247
    default:
        panic("non-exhaustive match")
    }
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline239)
    var t200 Result__string__string = use_handle(false, true)
    var t201 string
    switch t200.(type) {
    case Result__string__string_Ok:
        var inline232 string = t200.(Result__string__string_Ok)._0
        var inline234 string = "ok " + inline232
        t201 = inline234
    case Result__string__string_Err:
        var inline235 string = t200.(Result__string__string_Err)._0
        var inline237 string = "err " + inline235
        t201 = inline237
    default:
        panic("non-exhaustive match")
    }
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline229)
    var t202 Result__string__string = use_handle(true, false)
    var t203 string
    switch t202.(type) {
    case Result__string__string_Ok:
        var inline222 string = t202.(Result__string__string_Ok)._0
        var inline224 string = "ok " + inline222
        t203 = inline224
    case Result__string__string_Err:
        var inline225 string = t202.(Result__string__string_Err)._0
        var inline227 string = "err " + inline225
        t203 = inline227
    default:
        panic("non-exhaustive match")
    }
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline219)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
