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
    var mtmp187 Result__Handle__string
    if open_ok__3 {
        var inline247 Handle = Handle{
            name: "config",
        }
        var inline248 Result__Handle__string = Result__Handle__string_Ok{
            _0: inline247,
        }
        mtmp187 = inline248
    } else {
        var inline249 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        mtmp187 = inline249
    }
    var jp217 Handle
    switch mtmp187.(type) {
    case Result__Handle__string_Ok:
        var x188 Handle = mtmp187.(Result__Handle__string_Ok)._0
        jp217 = x188
        var name__6 string = jp217.name
        var mtmp190 Result__unit__string
        if close_ok__4 {
            var inline242 Result__unit__string = Result__unit__string_Ok{
                _0: struct{}{},
            }
            mtmp190 = inline242
        } else {
            var inline243 string = jp217.name
            var inline244 string = "close failed for " + inline243
            var inline245 Result__unit__string = Result__unit__string_Err{
                _0: inline244,
            }
            mtmp190 = inline245
        }
        switch mtmp190.(type) {
        case Result__unit__string_Ok:
            var t219 string = "closed " + name__6
            var t220 Result__string__string = Result__string__string_Ok{
                _0: t219,
            }
            return t220
        case Result__unit__string_Err:
            var x192 string = mtmp190.(Result__unit__string_Err)._0
            var t221 Result__string__string = Result__string__string_Err{
                _0: x192,
            }
            return t221
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x189 string = mtmp187.(Result__Handle__string_Err)._0
        var t222 Result__string__string = Result__string__string_Err{
            _0: x189,
        }
        return t222
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t230 Result__string__string = use_handle(true, true)
    var t231 string
    switch t230.(type) {
    case Result__string__string_Ok:
        var inline274 string = t230.(Result__string__string_Ok)._0
        var inline276 string = "ok " + inline274
        t231 = inline276
    case Result__string__string_Err:
        var inline277 string = t230.(Result__string__string_Err)._0
        var inline279 string = "err " + inline277
        t231 = inline279
    default:
        panic("non-exhaustive match")
    }
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t231)
    _goml_runtime_core_string_println(inline271)
    var t232 Result__string__string = use_handle(false, true)
    var t233 string
    switch t232.(type) {
    case Result__string__string_Ok:
        var inline264 string = t232.(Result__string__string_Ok)._0
        var inline266 string = "ok " + inline264
        t233 = inline266
    case Result__string__string_Err:
        var inline267 string = t232.(Result__string__string_Err)._0
        var inline269 string = "err " + inline267
        t233 = inline269
    default:
        panic("non-exhaustive match")
    }
    var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t233)
    _goml_runtime_core_string_println(inline261)
    var t234 Result__string__string = use_handle(true, false)
    var t235 string
    switch t234.(type) {
    case Result__string__string_Ok:
        var inline254 string = t234.(Result__string__string_Ok)._0
        var inline256 string = "ok " + inline254
        t235 = inline256
    case Result__string__string_Err:
        var inline257 string = t234.(Result__string__string_Err)._0
        var inline259 string = "err " + inline257
        t235 = inline259
    default:
        panic("non-exhaustive match")
    }
    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t235)
    _goml_runtime_core_string_println(inline251)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
