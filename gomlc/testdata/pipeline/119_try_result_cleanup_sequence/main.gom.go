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
    var mtmp172 Result__Handle__string
    if open_ok__3 {
        var inline232 Handle = Handle{
            name: "config",
        }
        var inline233 Result__Handle__string = Result__Handle__string_Ok{
            _0: inline232,
        }
        mtmp172 = inline233
    } else {
        var inline234 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        mtmp172 = inline234
    }
    var jp202 Handle
    switch mtmp172.(type) {
    case Result__Handle__string_Ok:
        var x173 Handle = mtmp172.(Result__Handle__string_Ok)._0
        jp202 = x173
        var name__6 string = jp202.name
        var mtmp175 Result__unit__string
        if close_ok__4 {
            var inline227 Result__unit__string = Result__unit__string_Ok{
                _0: struct{}{},
            }
            mtmp175 = inline227
        } else {
            var inline228 string = jp202.name
            var inline229 string = "close failed for " + inline228
            var inline230 Result__unit__string = Result__unit__string_Err{
                _0: inline229,
            }
            mtmp175 = inline230
        }
        switch mtmp175.(type) {
        case Result__unit__string_Ok:
            var t204 string = "closed " + name__6
            var t205 Result__string__string = Result__string__string_Ok{
                _0: t204,
            }
            return t205
        case Result__unit__string_Err:
            var x177 string = mtmp175.(Result__unit__string_Err)._0
            var t206 Result__string__string = Result__string__string_Err{
                _0: x177,
            }
            return t206
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x174 string = mtmp172.(Result__Handle__string_Err)._0
        var t207 Result__string__string = Result__string__string_Err{
            _0: x174,
        }
        return t207
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t215 Result__string__string = use_handle(true, true)
    var t216 string
    switch t215.(type) {
    case Result__string__string_Ok:
        var inline259 string = t215.(Result__string__string_Ok)._0
        var inline261 string = "ok " + inline259
        t216 = inline261
    case Result__string__string_Err:
        var inline262 string = t215.(Result__string__string_Err)._0
        var inline264 string = "err " + inline262
        t216 = inline264
    default:
        panic("non-exhaustive match")
    }
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline256)
    var t217 Result__string__string = use_handle(false, true)
    var t218 string
    switch t217.(type) {
    case Result__string__string_Ok:
        var inline249 string = t217.(Result__string__string_Ok)._0
        var inline251 string = "ok " + inline249
        t218 = inline251
    case Result__string__string_Err:
        var inline252 string = t217.(Result__string__string_Err)._0
        var inline254 string = "err " + inline252
        t218 = inline254
    default:
        panic("non-exhaustive match")
    }
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline246)
    var t219 Result__string__string = use_handle(true, false)
    var t220 string
    switch t219.(type) {
    case Result__string__string_Ok:
        var inline239 string = t219.(Result__string__string_Ok)._0
        var inline241 string = "ok " + inline239
        t220 = inline241
    case Result__string__string_Err:
        var inline242 string = t219.(Result__string__string_Err)._0
        var inline244 string = "err " + inline242
        t220 = inline244
    default:
        panic("non-exhaustive match")
    }
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
    _goml_runtime_core_string_println(inline236)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
