package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

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

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var mtmp187 Result__unit__string
    if config_ok__3 {
        var inline247 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        mtmp187 = inline247
    } else {
        var inline248 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        mtmp187 = inline248
    }
    switch mtmp187.(type) {
    case Result__unit__string_Ok:
        var mtmp191 Result__string__string
        if read_ok__4 {
            var inline244 Result__string__string = Result__string__string_Ok{
                _0: "2s",
            }
            mtmp191 = inline244
        } else {
            var inline245 Result__string__string = Result__string__string_Err{
                _0: "duration failed",
            }
            mtmp191 = inline245
        }
        var jp218 string
        switch mtmp191.(type) {
        case Result__string__string_Ok:
            var x192 string = mtmp191.(Result__string__string_Ok)._0
            jp218 = x192
            var t219 string
            var inline242 string = "duration=" + jp218
            t219 = inline242
            var t220 Result__string__string = Result__string__string_Ok{
                _0: t219,
            }
            return t220
        case Result__string__string_Err:
            var x193 string = mtmp191.(Result__string__string_Err)._0
            var t221 Result__string__string = Result__string__string_Err{
                _0: x193,
            }
            return t221
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x189 string = mtmp187.(Result__unit__string_Err)._0
        var t222 Result__string__string = Result__string__string_Err{
            _0: x189,
        }
        return t222
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t230 Result__string__string = configure_and_format(true, true)
    var t231 string
    switch t230.(type) {
    case Result__string__string_Ok:
        var inline273 string = t230.(Result__string__string_Ok)._0
        var inline275 string = "ok " + inline273
        t231 = inline275
    case Result__string__string_Err:
        var inline276 string = t230.(Result__string__string_Err)._0
        var inline278 string = "err " + inline276
        t231 = inline278
    default:
        panic("non-exhaustive match")
    }
    var inline270 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t231)
    _goml_runtime_core_string_println(inline270)
    var t232 Result__string__string = configure_and_format(true, false)
    var t233 string
    switch t232.(type) {
    case Result__string__string_Ok:
        var inline263 string = t232.(Result__string__string_Ok)._0
        var inline265 string = "ok " + inline263
        t233 = inline265
    case Result__string__string_Err:
        var inline266 string = t232.(Result__string__string_Err)._0
        var inline268 string = "err " + inline266
        t233 = inline268
    default:
        panic("non-exhaustive match")
    }
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t233)
    _goml_runtime_core_string_println(inline260)
    var t234 Result__string__string = configure_and_format(false, true)
    var t235 string
    switch t234.(type) {
    case Result__string__string_Ok:
        var inline253 string = t234.(Result__string__string_Ok)._0
        var inline255 string = "ok " + inline253
        t235 = inline255
    case Result__string__string_Err:
        var inline256 string = t234.(Result__string__string_Err)._0
        var inline258 string = "err " + inline256
        t235 = inline258
    default:
        panic("non-exhaustive match")
    }
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t235)
    _goml_runtime_core_string_println(inline250)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
