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
    var mtmp177 Result__Handle__string
    if open_ok__3 {
        var inline237 Handle = Handle{
            name: "config",
        }
        var inline238 Result__Handle__string = Result__Handle__string_Ok{
            _0: inline237,
        }
        mtmp177 = inline238
    } else {
        var inline239 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        mtmp177 = inline239
    }
    var jp207 Handle
    switch mtmp177.(type) {
    case Result__Handle__string_Ok:
        var x178 Handle = mtmp177.(Result__Handle__string_Ok)._0
        jp207 = x178
        var name__6 string = jp207.name
        var mtmp180 Result__unit__string
        if close_ok__4 {
            var inline232 Result__unit__string = Result__unit__string_Ok{
                _0: struct{}{},
            }
            mtmp180 = inline232
        } else {
            var inline233 string = jp207.name
            var inline234 string = "close failed for " + inline233
            var inline235 Result__unit__string = Result__unit__string_Err{
                _0: inline234,
            }
            mtmp180 = inline235
        }
        switch mtmp180.(type) {
        case Result__unit__string_Ok:
            var t209 string = "closed " + name__6
            var t210 Result__string__string = Result__string__string_Ok{
                _0: t209,
            }
            return t210
        case Result__unit__string_Err:
            var x182 string = mtmp180.(Result__unit__string_Err)._0
            var t211 Result__string__string = Result__string__string_Err{
                _0: x182,
            }
            return t211
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x179 string = mtmp177.(Result__Handle__string_Err)._0
        var t212 Result__string__string = Result__string__string_Err{
            _0: x179,
        }
        return t212
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t220 Result__string__string = use_handle(true, true)
    var t221 string
    switch t220.(type) {
    case Result__string__string_Ok:
        var inline264 string = t220.(Result__string__string_Ok)._0
        var inline266 string = "ok " + inline264
        t221 = inline266
    case Result__string__string_Err:
        var inline267 string = t220.(Result__string__string_Err)._0
        var inline269 string = "err " + inline267
        t221 = inline269
    default:
        panic("non-exhaustive match")
    }
    var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline261)
    var t222 Result__string__string = use_handle(false, true)
    var t223 string
    switch t222.(type) {
    case Result__string__string_Ok:
        var inline254 string = t222.(Result__string__string_Ok)._0
        var inline256 string = "ok " + inline254
        t223 = inline256
    case Result__string__string_Err:
        var inline257 string = t222.(Result__string__string_Err)._0
        var inline259 string = "err " + inline257
        t223 = inline259
    default:
        panic("non-exhaustive match")
    }
    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
    _goml_runtime_core_string_println(inline251)
    var t224 Result__string__string = use_handle(true, false)
    var t225 string
    switch t224.(type) {
    case Result__string__string_Ok:
        var inline244 string = t224.(Result__string__string_Ok)._0
        var inline246 string = "ok " + inline244
        t225 = inline246
    case Result__string__string_Err:
        var inline247 string = t224.(Result__string__string_Err)._0
        var inline249 string = "err " + inline247
        t225 = inline249
    default:
        panic("non-exhaustive match")
    }
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline241)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
