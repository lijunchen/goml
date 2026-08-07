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
    var mtmp172 Result__unit__string
    if config_ok__3 {
        var inline232 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        mtmp172 = inline232
    } else {
        var inline233 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        mtmp172 = inline233
    }
    switch mtmp172.(type) {
    case Result__unit__string_Ok:
        var mtmp176 Result__string__string
        if read_ok__4 {
            var inline229 Result__string__string = Result__string__string_Ok{
                _0: "2s",
            }
            mtmp176 = inline229
        } else {
            var inline230 Result__string__string = Result__string__string_Err{
                _0: "duration failed",
            }
            mtmp176 = inline230
        }
        var jp203 string
        switch mtmp176.(type) {
        case Result__string__string_Ok:
            var x177 string = mtmp176.(Result__string__string_Ok)._0
            jp203 = x177
            var t204 string
            var inline227 string = "duration=" + jp203
            t204 = inline227
            var t205 Result__string__string = Result__string__string_Ok{
                _0: t204,
            }
            return t205
        case Result__string__string_Err:
            var x178 string = mtmp176.(Result__string__string_Err)._0
            var t206 Result__string__string = Result__string__string_Err{
                _0: x178,
            }
            return t206
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x174 string = mtmp172.(Result__unit__string_Err)._0
        var t207 Result__string__string = Result__string__string_Err{
            _0: x174,
        }
        return t207
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t215 Result__string__string = configure_and_format(true, true)
    var t216 string
    switch t215.(type) {
    case Result__string__string_Ok:
        var inline258 string = t215.(Result__string__string_Ok)._0
        var inline260 string = "ok " + inline258
        t216 = inline260
    case Result__string__string_Err:
        var inline261 string = t215.(Result__string__string_Err)._0
        var inline263 string = "err " + inline261
        t216 = inline263
    default:
        panic("non-exhaustive match")
    }
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline255)
    var t217 Result__string__string = configure_and_format(true, false)
    var t218 string
    switch t217.(type) {
    case Result__string__string_Ok:
        var inline248 string = t217.(Result__string__string_Ok)._0
        var inline250 string = "ok " + inline248
        t218 = inline250
    case Result__string__string_Err:
        var inline251 string = t217.(Result__string__string_Err)._0
        var inline253 string = "err " + inline251
        t218 = inline253
    default:
        panic("non-exhaustive match")
    }
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline245)
    var t219 Result__string__string = configure_and_format(false, true)
    var t220 string
    switch t219.(type) {
    case Result__string__string_Ok:
        var inline238 string = t219.(Result__string__string_Ok)._0
        var inline240 string = "ok " + inline238
        t220 = inline240
    case Result__string__string_Err:
        var inline241 string = t219.(Result__string__string_Err)._0
        var inline243 string = "err " + inline241
        t220 = inline243
    default:
        panic("non-exhaustive match")
    }
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
    _goml_runtime_core_string_println(inline235)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
