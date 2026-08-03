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
        var t191 Result__string__string = Ok{
            _0: "goml",
        }
        return t191
    } else {
        var t192 Result__string__string = Err{
            _0: "parse failed",
        }
        return t192
    }
}

func normalize_text(ok__1 bool) Result__string__string {
    var mtmp177 Result__string__string
    if ok__1 {
        var inline225 Result__string__string = Ok{
            _0: "goml",
        }
        mtmp177 = inline225
    } else {
        var inline226 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp177 = inline226
    }
    var jp196 string
    switch mtmp177.(type) {
    case Ok:
        var x178 string = mtmp177.(Ok)._0
        jp196 = x178
        var t197 string = jp196 + "!"
        var t198 Result__string__string = Ok{
            _0: t197,
        }
        return t198
    case Err:
        var x179 string = mtmp177.(Err)._0
        var t199 Result__string__string = Err{
            _0: x179,
        }
        return t199
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var mtmp180 Result__string__string
    var inline228 Result__string__string = parse_text(ok__3)
    var inline230 string
    switch inline228.(type) {
    case Ok:
        var inline234 string = inline228.(Ok)._0
        inline230 = inline234
        var inline232 string = inline230 + "!"
        var inline233 Result__string__string = Ok{
            _0: inline232,
        }
        mtmp180 = inline233
        var jp203 string
        switch mtmp180.(type) {
        case Ok:
            var x181 string = mtmp180.(Ok)._0
            jp203 = x181
            var t204 string = "[" + jp203
            var t205 string = t204 + "]"
            var t206 Result__string__string = Ok{
                _0: t205,
            }
            return t206
        case Err:
            var x182 string = mtmp180.(Err)._0
            var t207 Result__string__string = Err{
                _0: x182,
            }
            return t207
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline236 string = inline228.(Err)._0
        var inline238 Result__string__string = Err{
            _0: inline236,
        }
        mtmp180 = inline238
        var jp203 string
        switch mtmp180.(type) {
        case Ok:
            var x181 string = mtmp180.(Ok)._0
            jp203 = x181
            var t204 string = "[" + jp203
            var t205 string = t204 + "]"
            var t206 Result__string__string = Ok{
                _0: t205,
            }
            return t206
        case Err:
            var x182 string = mtmp180.(Err)._0
            var t207 Result__string__string = Err{
                _0: x182,
            }
            return t207
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t215 Result__string__string = decorate_text(true)
    var t216 string
    switch t215.(type) {
    case Ok:
        var inline267 string = t215.(Ok)._0
        var inline269 string = "ok " + inline267
        t216 = inline269
    case Err:
        var inline270 string = t215.(Err)._0
        var inline272 string = "err " + inline270
        t216 = inline272
    default:
        panic("non-exhaustive match")
    }
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline264)
    var t217 Result__string__string
    var inline250 bool = false
    var inline251 Result__string__string = normalize_text(inline250)
    var inline253 string
    switch inline251.(type) {
    case Ok:
        var inline258 string = inline251.(Ok)._0
        inline253 = inline258
        var inline255 string = "[" + inline253
        var inline256 string = inline255 + "]"
        var inline257 Result__string__string = Ok{
            _0: inline256,
        }
        t217 = inline257
        var t218 string
        switch t217.(type) {
        case Ok:
            var inline243 string = t217.(Ok)._0
            var inline245 string = "ok " + inline243
            t218 = inline245
        case Err:
            var inline246 string = t217.(Err)._0
            var inline248 string = "err " + inline246
            t218 = inline248
        default:
            panic("non-exhaustive match")
        }
        var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
        _goml_runtime_core_string_println(inline240)
        return struct{}{}
    case Err:
        var inline260 string = inline251.(Err)._0
        var inline262 Result__string__string = Err{
            _0: inline260,
        }
        t217 = inline262
        var t218 string
        switch t217.(type) {
        case Ok:
            var inline243 string = t217.(Ok)._0
            var inline245 string = "ok " + inline243
            t218 = inline245
        case Err:
            var inline246 string = t217.(Err)._0
            var inline248 string = "err " + inline246
            t218 = inline248
        default:
            panic("non-exhaustive match")
        }
        var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
        _goml_runtime_core_string_println(inline240)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
