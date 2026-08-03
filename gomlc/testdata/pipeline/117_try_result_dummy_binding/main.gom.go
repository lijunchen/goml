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
    var mtmp177 Result__unit__string
    if config_ok__3 {
        var inline237 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        mtmp177 = inline237
    } else {
        var inline238 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        mtmp177 = inline238
    }
    switch mtmp177.(type) {
    case Result__unit__string_Ok:
        var mtmp181 Result__string__string
        if read_ok__4 {
            var inline234 Result__string__string = Result__string__string_Ok{
                _0: "2s",
            }
            mtmp181 = inline234
        } else {
            var inline235 Result__string__string = Result__string__string_Err{
                _0: "duration failed",
            }
            mtmp181 = inline235
        }
        var jp208 string
        switch mtmp181.(type) {
        case Result__string__string_Ok:
            var x182 string = mtmp181.(Result__string__string_Ok)._0
            jp208 = x182
            var t209 string
            var inline232 string = "duration=" + jp208
            t209 = inline232
            var t210 Result__string__string = Result__string__string_Ok{
                _0: t209,
            }
            return t210
        case Result__string__string_Err:
            var x183 string = mtmp181.(Result__string__string_Err)._0
            var t211 Result__string__string = Result__string__string_Err{
                _0: x183,
            }
            return t211
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x179 string = mtmp177.(Result__unit__string_Err)._0
        var t212 Result__string__string = Result__string__string_Err{
            _0: x179,
        }
        return t212
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t220 Result__string__string = configure_and_format(true, true)
    var t221 string
    switch t220.(type) {
    case Result__string__string_Ok:
        var inline263 string = t220.(Result__string__string_Ok)._0
        var inline265 string = "ok " + inline263
        t221 = inline265
    case Result__string__string_Err:
        var inline266 string = t220.(Result__string__string_Err)._0
        var inline268 string = "err " + inline266
        t221 = inline268
    default:
        panic("non-exhaustive match")
    }
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline260)
    var t222 Result__string__string = configure_and_format(true, false)
    var t223 string
    switch t222.(type) {
    case Result__string__string_Ok:
        var inline253 string = t222.(Result__string__string_Ok)._0
        var inline255 string = "ok " + inline253
        t223 = inline255
    case Result__string__string_Err:
        var inline256 string = t222.(Result__string__string_Err)._0
        var inline258 string = "err " + inline256
        t223 = inline258
    default:
        panic("non-exhaustive match")
    }
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
    _goml_runtime_core_string_println(inline250)
    var t224 Result__string__string = configure_and_format(false, true)
    var t225 string
    switch t224.(type) {
    case Result__string__string_Ok:
        var inline243 string = t224.(Result__string__string_Ok)._0
        var inline245 string = "ok " + inline243
        t225 = inline245
    case Result__string__string_Err:
        var inline246 string = t224.(Result__string__string_Err)._0
        var inline248 string = "err " + inline246
        t225 = inline248
    default:
        panic("non-exhaustive match")
    }
    var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline240)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
