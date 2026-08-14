package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Result__string__string interface {
    isResult__string__string()
}

type Ok struct {
    _0 string
}

func (_ Ok) isResult__string__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__string__string() {}

func parse_text(ok__0 bool) Result__string__string {
    if ok__0 {
        var t201 Result__string__string = Ok{
            _0: "goml",
        }
        return t201
    } else {
        var t202 Result__string__string = Err{
            _0: "parse failed",
        }
        return t202
    }
}

func normalize_text(ok__1 bool) Result__string__string {
    var mtmp187 Result__string__string
    if ok__1 {
        var inline235 Result__string__string = Ok{
            _0: "goml",
        }
        mtmp187 = inline235
    } else {
        var inline236 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp187 = inline236
    }
    var jp206 string
    switch mtmp187.(type) {
    case Ok:
        var x188 string = mtmp187.(Ok)._0
        jp206 = x188
        var t207 string = jp206 + "!"
        var t208 Result__string__string = Ok{
            _0: t207,
        }
        return t208
    case Err:
        var x189 string = mtmp187.(Err)._0
        var t209 Result__string__string = Err{
            _0: x189,
        }
        return t209
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var mtmp190 Result__string__string
    var inline238 Result__string__string = parse_text(ok__3)
    var inline240 string
    switch inline238.(type) {
    case Ok:
        var inline244 string = inline238.(Ok)._0
        inline240 = inline244
        var inline242 string = inline240 + "!"
        var inline243 Result__string__string = Ok{
            _0: inline242,
        }
        mtmp190 = inline243
        var jp213 string
        switch mtmp190.(type) {
        case Ok:
            var x191 string = mtmp190.(Ok)._0
            jp213 = x191
            var t214 string = "[" + jp213
            var t215 string = t214 + "]"
            var t216 Result__string__string = Ok{
                _0: t215,
            }
            return t216
        case Err:
            var x192 string = mtmp190.(Err)._0
            var t217 Result__string__string = Err{
                _0: x192,
            }
            return t217
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline246 string = inline238.(Err)._0
        var inline248 Result__string__string = Err{
            _0: inline246,
        }
        mtmp190 = inline248
        var jp213 string
        switch mtmp190.(type) {
        case Ok:
            var x191 string = mtmp190.(Ok)._0
            jp213 = x191
            var t214 string = "[" + jp213
            var t215 string = t214 + "]"
            var t216 Result__string__string = Ok{
                _0: t215,
            }
            return t216
        case Err:
            var x192 string = mtmp190.(Err)._0
            var t217 Result__string__string = Err{
                _0: x192,
            }
            return t217
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t225 Result__string__string = decorate_text(true)
    var t226 string
    switch t225.(type) {
    case Ok:
        var inline277 string = t225.(Ok)._0
        var inline279 string = "ok " + inline277
        t226 = inline279
    case Err:
        var inline280 string = t225.(Err)._0
        var inline282 string = "err " + inline280
        t226 = inline282
    default:
        panic("non-exhaustive match")
    }
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline274)
    var t227 Result__string__string
    var inline260 bool = false
    var inline261 Result__string__string = normalize_text(inline260)
    var inline263 string
    switch inline261.(type) {
    case Ok:
        var inline268 string = inline261.(Ok)._0
        inline263 = inline268
        var inline265 string = "[" + inline263
        var inline266 string = inline265 + "]"
        var inline267 Result__string__string = Ok{
            _0: inline266,
        }
        t227 = inline267
        var t228 string
        switch t227.(type) {
        case Ok:
            var inline253 string = t227.(Ok)._0
            var inline255 string = "ok " + inline253
            t228 = inline255
        case Err:
            var inline256 string = t227.(Err)._0
            var inline258 string = "err " + inline256
            t228 = inline258
        default:
            panic("non-exhaustive match")
        }
        var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
        _goml_runtime_core_string_println(inline250)
        return struct{}{}
    case Err:
        var inline270 string = inline261.(Err)._0
        var inline272 Result__string__string = Err{
            _0: inline270,
        }
        t227 = inline272
        var t228 string
        switch t227.(type) {
        case Ok:
            var inline253 string = t227.(Ok)._0
            var inline255 string = "ok " + inline253
            t228 = inline255
        case Err:
            var inline256 string = t227.(Err)._0
            var inline258 string = "err " + inline256
            t228 = inline258
        default:
            panic("non-exhaustive match")
        }
        var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
        _goml_runtime_core_string_println(inline250)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
