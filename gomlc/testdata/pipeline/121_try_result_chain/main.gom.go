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
        var t186 Result__string__string = Ok{
            _0: "goml",
        }
        return t186
    } else {
        var t187 Result__string__string = Err{
            _0: "parse failed",
        }
        return t187
    }
}

func normalize_text(ok__1 bool) Result__string__string {
    var mtmp172 Result__string__string
    if ok__1 {
        var inline220 Result__string__string = Ok{
            _0: "goml",
        }
        mtmp172 = inline220
    } else {
        var inline221 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp172 = inline221
    }
    var jp191 string
    switch mtmp172.(type) {
    case Ok:
        var x173 string = mtmp172.(Ok)._0
        jp191 = x173
        var t192 string = jp191 + "!"
        var t193 Result__string__string = Ok{
            _0: t192,
        }
        return t193
    case Err:
        var x174 string = mtmp172.(Err)._0
        var t194 Result__string__string = Err{
            _0: x174,
        }
        return t194
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var mtmp175 Result__string__string
    var inline223 Result__string__string = parse_text(ok__3)
    var inline225 string
    switch inline223.(type) {
    case Ok:
        var inline229 string = inline223.(Ok)._0
        inline225 = inline229
        var inline227 string = inline225 + "!"
        var inline228 Result__string__string = Ok{
            _0: inline227,
        }
        mtmp175 = inline228
        var jp198 string
        switch mtmp175.(type) {
        case Ok:
            var x176 string = mtmp175.(Ok)._0
            jp198 = x176
            var t199 string = "[" + jp198
            var t200 string = t199 + "]"
            var t201 Result__string__string = Ok{
                _0: t200,
            }
            return t201
        case Err:
            var x177 string = mtmp175.(Err)._0
            var t202 Result__string__string = Err{
                _0: x177,
            }
            return t202
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline231 string = inline223.(Err)._0
        var inline233 Result__string__string = Err{
            _0: inline231,
        }
        mtmp175 = inline233
        var jp198 string
        switch mtmp175.(type) {
        case Ok:
            var x176 string = mtmp175.(Ok)._0
            jp198 = x176
            var t199 string = "[" + jp198
            var t200 string = t199 + "]"
            var t201 Result__string__string = Ok{
                _0: t200,
            }
            return t201
        case Err:
            var x177 string = mtmp175.(Err)._0
            var t202 Result__string__string = Err{
                _0: x177,
            }
            return t202
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t210 Result__string__string = decorate_text(true)
    var t211 string
    switch t210.(type) {
    case Ok:
        var inline262 string = t210.(Ok)._0
        var inline264 string = "ok " + inline262
        t211 = inline264
    case Err:
        var inline265 string = t210.(Err)._0
        var inline267 string = "err " + inline265
        t211 = inline267
    default:
        panic("non-exhaustive match")
    }
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline259)
    var t212 Result__string__string
    var inline245 bool = false
    var inline246 Result__string__string = normalize_text(inline245)
    var inline248 string
    switch inline246.(type) {
    case Ok:
        var inline253 string = inline246.(Ok)._0
        inline248 = inline253
        var inline250 string = "[" + inline248
        var inline251 string = inline250 + "]"
        var inline252 Result__string__string = Ok{
            _0: inline251,
        }
        t212 = inline252
        var t213 string
        switch t212.(type) {
        case Ok:
            var inline238 string = t212.(Ok)._0
            var inline240 string = "ok " + inline238
            t213 = inline240
        case Err:
            var inline241 string = t212.(Err)._0
            var inline243 string = "err " + inline241
            t213 = inline243
        default:
            panic("non-exhaustive match")
        }
        var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
        _goml_runtime_core_string_println(inline235)
        return struct{}{}
    case Err:
        var inline255 string = inline246.(Err)._0
        var inline257 Result__string__string = Err{
            _0: inline255,
        }
        t212 = inline257
        var t213 string
        switch t212.(type) {
        case Ok:
            var inline238 string = t212.(Ok)._0
            var inline240 string = "ok " + inline238
            t213 = inline240
        case Err:
            var inline241 string = t212.(Err)._0
            var inline243 string = "err " + inline241
            t213 = inline243
        default:
            panic("non-exhaustive match")
        }
        var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
        _goml_runtime_core_string_println(inline235)
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
