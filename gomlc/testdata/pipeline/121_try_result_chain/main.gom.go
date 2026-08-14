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
        var t196 Result__string__string = Ok{
            _0: "goml",
        }
        return t196
    } else {
        var t197 Result__string__string = Err{
            _0: "parse failed",
        }
        return t197
    }
}

func normalize_text(ok__1 bool) Result__string__string {
    var mtmp182 Result__string__string
    if ok__1 {
        var inline230 Result__string__string = Ok{
            _0: "goml",
        }
        mtmp182 = inline230
    } else {
        var inline231 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp182 = inline231
    }
    var jp201 string
    switch mtmp182.(type) {
    case Ok:
        var x183 string = mtmp182.(Ok)._0
        jp201 = x183
        var t202 string = jp201 + "!"
        var t203 Result__string__string = Ok{
            _0: t202,
        }
        return t203
    case Err:
        var x184 string = mtmp182.(Err)._0
        var t204 Result__string__string = Err{
            _0: x184,
        }
        return t204
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var mtmp185 Result__string__string
    var inline233 Result__string__string = parse_text(ok__3)
    var inline235 string
    switch inline233.(type) {
    case Ok:
        var inline239 string = inline233.(Ok)._0
        inline235 = inline239
        var inline237 string = inline235 + "!"
        var inline238 Result__string__string = Ok{
            _0: inline237,
        }
        mtmp185 = inline238
        var jp208 string
        switch mtmp185.(type) {
        case Ok:
            var x186 string = mtmp185.(Ok)._0
            jp208 = x186
            var t209 string = "[" + jp208
            var t210 string = t209 + "]"
            var t211 Result__string__string = Ok{
                _0: t210,
            }
            return t211
        case Err:
            var x187 string = mtmp185.(Err)._0
            var t212 Result__string__string = Err{
                _0: x187,
            }
            return t212
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline241 string = inline233.(Err)._0
        var inline243 Result__string__string = Err{
            _0: inline241,
        }
        mtmp185 = inline243
        var jp208 string
        switch mtmp185.(type) {
        case Ok:
            var x186 string = mtmp185.(Ok)._0
            jp208 = x186
            var t209 string = "[" + jp208
            var t210 string = t209 + "]"
            var t211 Result__string__string = Ok{
                _0: t210,
            }
            return t211
        case Err:
            var x187 string = mtmp185.(Err)._0
            var t212 Result__string__string = Err{
                _0: x187,
            }
            return t212
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t220 Result__string__string = decorate_text(true)
    var t221 string
    switch t220.(type) {
    case Ok:
        var inline272 string = t220.(Ok)._0
        var inline274 string = "ok " + inline272
        t221 = inline274
    case Err:
        var inline275 string = t220.(Err)._0
        var inline277 string = "err " + inline275
        t221 = inline277
    default:
        panic("non-exhaustive match")
    }
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline269)
    var t222 Result__string__string
    var inline255 bool = false
    var inline256 Result__string__string = normalize_text(inline255)
    var inline258 string
    switch inline256.(type) {
    case Ok:
        var inline263 string = inline256.(Ok)._0
        inline258 = inline263
        var inline260 string = "[" + inline258
        var inline261 string = inline260 + "]"
        var inline262 Result__string__string = Ok{
            _0: inline261,
        }
        t222 = inline262
        var t223 string
        switch t222.(type) {
        case Ok:
            var inline248 string = t222.(Ok)._0
            var inline250 string = "ok " + inline248
            t223 = inline250
        case Err:
            var inline251 string = t222.(Err)._0
            var inline253 string = "err " + inline251
            t223 = inline253
        default:
            panic("non-exhaustive match")
        }
        var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
        _goml_runtime_core_string_println(inline245)
        return struct{}{}
    case Err:
        var inline265 string = inline256.(Err)._0
        var inline267 Result__string__string = Err{
            _0: inline265,
        }
        t222 = inline267
        var t223 string
        switch t222.(type) {
        case Ok:
            var inline248 string = t222.(Ok)._0
            var inline250 string = "ok " + inline248
            t223 = inline250
        case Err:
            var inline251 string = t222.(Err)._0
            var inline253 string = "err " + inline251
            t223 = inline253
        default:
            panic("non-exhaustive match")
        }
        var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
        _goml_runtime_core_string_println(inline245)
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
