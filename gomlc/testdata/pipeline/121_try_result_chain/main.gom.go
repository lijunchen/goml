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
        var t169 Result__string__string = Ok{
            _0: "goml",
        }
        return t169
    } else {
        var t170 Result__string__string = Err{
            _0: "parse failed",
        }
        return t170
    }
}

func normalize_text(ok__1 bool) Result__string__string {
    var mtmp155 Result__string__string
    if ok__1 {
        var inline203 Result__string__string = Ok{
            _0: "goml",
        }
        mtmp155 = inline203
    } else {
        var inline204 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp155 = inline204
    }
    var jp174 string
    switch mtmp155.(type) {
    case Ok:
        var x156 string = mtmp155.(Ok)._0
        jp174 = x156
        var t175 string = jp174 + "!"
        var t176 Result__string__string = Ok{
            _0: t175,
        }
        return t176
    case Err:
        var x157 string = mtmp155.(Err)._0
        var t177 Result__string__string = Err{
            _0: x157,
        }
        return t177
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var mtmp158 Result__string__string
    var inline206 Result__string__string = parse_text(ok__3)
    var inline208 string
    switch inline206.(type) {
    case Ok:
        var inline212 string = inline206.(Ok)._0
        inline208 = inline212
        var inline210 string = inline208 + "!"
        var inline211 Result__string__string = Ok{
            _0: inline210,
        }
        mtmp158 = inline211
        var jp181 string
        switch mtmp158.(type) {
        case Ok:
            var x159 string = mtmp158.(Ok)._0
            jp181 = x159
            var t182 string = "[" + jp181
            var t183 string = t182 + "]"
            var t184 Result__string__string = Ok{
                _0: t183,
            }
            return t184
        case Err:
            var x160 string = mtmp158.(Err)._0
            var t185 Result__string__string = Err{
                _0: x160,
            }
            return t185
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline214 string = inline206.(Err)._0
        var inline216 Result__string__string = Err{
            _0: inline214,
        }
        mtmp158 = inline216
        var jp181 string
        switch mtmp158.(type) {
        case Ok:
            var x159 string = mtmp158.(Ok)._0
            jp181 = x159
            var t182 string = "[" + jp181
            var t183 string = t182 + "]"
            var t184 Result__string__string = Ok{
                _0: t183,
            }
            return t184
        case Err:
            var x160 string = mtmp158.(Err)._0
            var t185 Result__string__string = Err{
                _0: x160,
            }
            return t185
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t193 Result__string__string = decorate_text(true)
    var t194 string
    switch t193.(type) {
    case Ok:
        var inline245 string = t193.(Ok)._0
        var inline247 string = "ok " + inline245
        t194 = inline247
    case Err:
        var inline248 string = t193.(Err)._0
        var inline250 string = "err " + inline248
        t194 = inline250
    default:
        panic("non-exhaustive match")
    }
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline242)
    var t195 Result__string__string
    var inline228 bool = false
    var inline229 Result__string__string = normalize_text(inline228)
    var inline231 string
    switch inline229.(type) {
    case Ok:
        var inline236 string = inline229.(Ok)._0
        inline231 = inline236
        var inline233 string = "[" + inline231
        var inline234 string = inline233 + "]"
        var inline235 Result__string__string = Ok{
            _0: inline234,
        }
        t195 = inline235
        var t196 string
        switch t195.(type) {
        case Ok:
            var inline221 string = t195.(Ok)._0
            var inline223 string = "ok " + inline221
            t196 = inline223
        case Err:
            var inline224 string = t195.(Err)._0
            var inline226 string = "err " + inline224
            t196 = inline226
        default:
            panic("non-exhaustive match")
        }
        var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
        _goml_runtime_core_string_println(inline218)
        return struct{}{}
    case Err:
        var inline238 string = inline229.(Err)._0
        var inline240 Result__string__string = Err{
            _0: inline238,
        }
        t195 = inline240
        var t196 string
        switch t195.(type) {
        case Ok:
            var inline221 string = t195.(Ok)._0
            var inline223 string = "ok " + inline221
            t196 = inline223
        case Err:
            var inline224 string = t195.(Err)._0
            var inline226 string = "err " + inline224
            t196 = inline226
        default:
            panic("non-exhaustive match")
        }
        var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
        _goml_runtime_core_string_println(inline218)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
