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
    var mtmp155 Result__unit__string
    if config_ok__3 {
        var inline215 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        mtmp155 = inline215
    } else {
        var inline216 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        mtmp155 = inline216
    }
    switch mtmp155.(type) {
    case Result__unit__string_Ok:
        var mtmp159 Result__string__string
        if read_ok__4 {
            var inline212 Result__string__string = Result__string__string_Ok{
                _0: "2s",
            }
            mtmp159 = inline212
        } else {
            var inline213 Result__string__string = Result__string__string_Err{
                _0: "duration failed",
            }
            mtmp159 = inline213
        }
        var jp186 string
        switch mtmp159.(type) {
        case Result__string__string_Ok:
            var x160 string = mtmp159.(Result__string__string_Ok)._0
            jp186 = x160
            var t187 string
            var inline210 string = "duration=" + jp186
            t187 = inline210
            var t188 Result__string__string = Result__string__string_Ok{
                _0: t187,
            }
            return t188
        case Result__string__string_Err:
            var x161 string = mtmp159.(Result__string__string_Err)._0
            var t189 Result__string__string = Result__string__string_Err{
                _0: x161,
            }
            return t189
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x157 string = mtmp155.(Result__unit__string_Err)._0
        var t190 Result__string__string = Result__string__string_Err{
            _0: x157,
        }
        return t190
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t198 Result__string__string = configure_and_format(true, true)
    var t199 string
    switch t198.(type) {
    case Result__string__string_Ok:
        var inline241 string = t198.(Result__string__string_Ok)._0
        var inline243 string = "ok " + inline241
        t199 = inline243
    case Result__string__string_Err:
        var inline244 string = t198.(Result__string__string_Err)._0
        var inline246 string = "err " + inline244
        t199 = inline246
    default:
        panic("non-exhaustive match")
    }
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline238)
    var t200 Result__string__string = configure_and_format(true, false)
    var t201 string
    switch t200.(type) {
    case Result__string__string_Ok:
        var inline231 string = t200.(Result__string__string_Ok)._0
        var inline233 string = "ok " + inline231
        t201 = inline233
    case Result__string__string_Err:
        var inline234 string = t200.(Result__string__string_Err)._0
        var inline236 string = "err " + inline234
        t201 = inline236
    default:
        panic("non-exhaustive match")
    }
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline228)
    var t202 Result__string__string = configure_and_format(false, true)
    var t203 string
    switch t202.(type) {
    case Result__string__string_Ok:
        var inline221 string = t202.(Result__string__string_Ok)._0
        var inline223 string = "ok " + inline221
        t203 = inline223
    case Result__string__string_Err:
        var inline224 string = t202.(Result__string__string_Err)._0
        var inline226 string = "err " + inline224
        t203 = inline226
    default:
        panic("non-exhaustive match")
    }
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline218)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
