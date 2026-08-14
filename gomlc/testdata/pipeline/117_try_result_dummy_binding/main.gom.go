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
    var mtmp182 Result__unit__string
    if config_ok__3 {
        var inline242 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        mtmp182 = inline242
    } else {
        var inline243 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        mtmp182 = inline243
    }
    switch mtmp182.(type) {
    case Result__unit__string_Ok:
        var mtmp186 Result__string__string
        if read_ok__4 {
            var inline239 Result__string__string = Result__string__string_Ok{
                _0: "2s",
            }
            mtmp186 = inline239
        } else {
            var inline240 Result__string__string = Result__string__string_Err{
                _0: "duration failed",
            }
            mtmp186 = inline240
        }
        var jp213 string
        switch mtmp186.(type) {
        case Result__string__string_Ok:
            var x187 string = mtmp186.(Result__string__string_Ok)._0
            jp213 = x187
            var t214 string
            var inline237 string = "duration=" + jp213
            t214 = inline237
            var t215 Result__string__string = Result__string__string_Ok{
                _0: t214,
            }
            return t215
        case Result__string__string_Err:
            var x188 string = mtmp186.(Result__string__string_Err)._0
            var t216 Result__string__string = Result__string__string_Err{
                _0: x188,
            }
            return t216
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x184 string = mtmp182.(Result__unit__string_Err)._0
        var t217 Result__string__string = Result__string__string_Err{
            _0: x184,
        }
        return t217
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t225 Result__string__string = configure_and_format(true, true)
    var t226 string
    switch t225.(type) {
    case Result__string__string_Ok:
        var inline268 string = t225.(Result__string__string_Ok)._0
        var inline270 string = "ok " + inline268
        t226 = inline270
    case Result__string__string_Err:
        var inline271 string = t225.(Result__string__string_Err)._0
        var inline273 string = "err " + inline271
        t226 = inline273
    default:
        panic("non-exhaustive match")
    }
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline265)
    var t227 Result__string__string = configure_and_format(true, false)
    var t228 string
    switch t227.(type) {
    case Result__string__string_Ok:
        var inline258 string = t227.(Result__string__string_Ok)._0
        var inline260 string = "ok " + inline258
        t228 = inline260
    case Result__string__string_Err:
        var inline261 string = t227.(Result__string__string_Err)._0
        var inline263 string = "err " + inline261
        t228 = inline263
    default:
        panic("non-exhaustive match")
    }
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline255)
    var t229 Result__string__string = configure_and_format(false, true)
    var t230 string
    switch t229.(type) {
    case Result__string__string_Ok:
        var inline248 string = t229.(Result__string__string_Ok)._0
        var inline250 string = "ok " + inline248
        t230 = inline250
    case Result__string__string_Err:
        var inline251 string = t229.(Result__string__string_Err)._0
        var inline253 string = "err " + inline251
        t230 = inline253
    default:
        panic("non-exhaustive match")
    }
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline245)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
