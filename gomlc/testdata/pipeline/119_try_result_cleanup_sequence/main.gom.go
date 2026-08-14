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
    var mtmp182 Result__Handle__string
    if open_ok__3 {
        var inline242 Handle = Handle{
            name: "config",
        }
        var inline243 Result__Handle__string = Result__Handle__string_Ok{
            _0: inline242,
        }
        mtmp182 = inline243
    } else {
        var inline244 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        mtmp182 = inline244
    }
    var jp212 Handle
    switch mtmp182.(type) {
    case Result__Handle__string_Ok:
        var x183 Handle = mtmp182.(Result__Handle__string_Ok)._0
        jp212 = x183
        var name__6 string = jp212.name
        var mtmp185 Result__unit__string
        if close_ok__4 {
            var inline237 Result__unit__string = Result__unit__string_Ok{
                _0: struct{}{},
            }
            mtmp185 = inline237
        } else {
            var inline238 string = jp212.name
            var inline239 string = "close failed for " + inline238
            var inline240 Result__unit__string = Result__unit__string_Err{
                _0: inline239,
            }
            mtmp185 = inline240
        }
        switch mtmp185.(type) {
        case Result__unit__string_Ok:
            var t214 string = "closed " + name__6
            var t215 Result__string__string = Result__string__string_Ok{
                _0: t214,
            }
            return t215
        case Result__unit__string_Err:
            var x187 string = mtmp185.(Result__unit__string_Err)._0
            var t216 Result__string__string = Result__string__string_Err{
                _0: x187,
            }
            return t216
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x184 string = mtmp182.(Result__Handle__string_Err)._0
        var t217 Result__string__string = Result__string__string_Err{
            _0: x184,
        }
        return t217
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t225 Result__string__string = use_handle(true, true)
    var t226 string
    switch t225.(type) {
    case Result__string__string_Ok:
        var inline269 string = t225.(Result__string__string_Ok)._0
        var inline271 string = "ok " + inline269
        t226 = inline271
    case Result__string__string_Err:
        var inline272 string = t225.(Result__string__string_Err)._0
        var inline274 string = "err " + inline272
        t226 = inline274
    default:
        panic("non-exhaustive match")
    }
    var inline266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline266)
    var t227 Result__string__string = use_handle(false, true)
    var t228 string
    switch t227.(type) {
    case Result__string__string_Ok:
        var inline259 string = t227.(Result__string__string_Ok)._0
        var inline261 string = "ok " + inline259
        t228 = inline261
    case Result__string__string_Err:
        var inline262 string = t227.(Result__string__string_Err)._0
        var inline264 string = "err " + inline262
        t228 = inline264
    default:
        panic("non-exhaustive match")
    }
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline256)
    var t229 Result__string__string = use_handle(true, false)
    var t230 string
    switch t229.(type) {
    case Result__string__string_Ok:
        var inline249 string = t229.(Result__string__string_Ok)._0
        var inline251 string = "ok " + inline249
        t230 = inline251
    case Result__string__string_Err:
        var inline252 string = t229.(Result__string__string_Err)._0
        var inline254 string = "err " + inline252
        t230 = inline254
    default:
        panic("non-exhaustive match")
    }
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline246)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
