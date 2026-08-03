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
        var t150 Result__string__string = Ok{
            _0: "goml",
        }
        return t150
    } else {
        var t151 Result__string__string = Err{
            _0: "parse failed",
        }
        return t151
    }
}

func normalize_text(ok__1 bool) Result__string__string {
    var mtmp136 Result__string__string
    if ok__1 {
        var inline184 Result__string__string = Ok{
            _0: "goml",
        }
        mtmp136 = inline184
    } else {
        var inline185 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp136 = inline185
    }
    var jp155 string
    switch mtmp136.(type) {
    case Ok:
        var x137 string = mtmp136.(Ok)._0
        jp155 = x137
        var t156 string = jp155 + "!"
        var t157 Result__string__string = Ok{
            _0: t156,
        }
        return t157
    case Err:
        var x138 string = mtmp136.(Err)._0
        var t158 Result__string__string = Err{
            _0: x138,
        }
        return t158
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var mtmp139 Result__string__string
    var inline187 Result__string__string = parse_text(ok__3)
    var inline189 string
    switch inline187.(type) {
    case Ok:
        var inline193 string = inline187.(Ok)._0
        inline189 = inline193
        var inline191 string = inline189 + "!"
        var inline192 Result__string__string = Ok{
            _0: inline191,
        }
        mtmp139 = inline192
        var jp162 string
        switch mtmp139.(type) {
        case Ok:
            var x140 string = mtmp139.(Ok)._0
            jp162 = x140
            var t163 string = "[" + jp162
            var t164 string = t163 + "]"
            var t165 Result__string__string = Ok{
                _0: t164,
            }
            return t165
        case Err:
            var x141 string = mtmp139.(Err)._0
            var t166 Result__string__string = Err{
                _0: x141,
            }
            return t166
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline195 string = inline187.(Err)._0
        var inline197 Result__string__string = Err{
            _0: inline195,
        }
        mtmp139 = inline197
        var jp162 string
        switch mtmp139.(type) {
        case Ok:
            var x140 string = mtmp139.(Ok)._0
            jp162 = x140
            var t163 string = "[" + jp162
            var t164 string = t163 + "]"
            var t165 Result__string__string = Ok{
                _0: t164,
            }
            return t165
        case Err:
            var x141 string = mtmp139.(Err)._0
            var t166 Result__string__string = Err{
                _0: x141,
            }
            return t166
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t174 Result__string__string = decorate_text(true)
    var t175 string
    switch t174.(type) {
    case Ok:
        var inline226 string = t174.(Ok)._0
        var inline228 string = "ok " + inline226
        t175 = inline228
    case Err:
        var inline229 string = t174.(Err)._0
        var inline231 string = "err " + inline229
        t175 = inline231
    default:
        panic("non-exhaustive match")
    }
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t175)
    _goml_runtime_core_string_println(inline223)
    var t176 Result__string__string
    var inline209 bool = false
    var inline210 Result__string__string = normalize_text(inline209)
    var inline212 string
    switch inline210.(type) {
    case Ok:
        var inline217 string = inline210.(Ok)._0
        inline212 = inline217
        var inline214 string = "[" + inline212
        var inline215 string = inline214 + "]"
        var inline216 Result__string__string = Ok{
            _0: inline215,
        }
        t176 = inline216
        var t177 string
        switch t176.(type) {
        case Ok:
            var inline202 string = t176.(Ok)._0
            var inline204 string = "ok " + inline202
            t177 = inline204
        case Err:
            var inline205 string = t176.(Err)._0
            var inline207 string = "err " + inline205
            t177 = inline207
        default:
            panic("non-exhaustive match")
        }
        var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
        _goml_runtime_core_string_println(inline199)
        return struct{}{}
    case Err:
        var inline219 string = inline210.(Err)._0
        var inline221 Result__string__string = Err{
            _0: inline219,
        }
        t176 = inline221
        var t177 string
        switch t176.(type) {
        case Ok:
            var inline202 string = t176.(Ok)._0
            var inline204 string = "ok " + inline202
            t177 = inline204
        case Err:
            var inline205 string = t176.(Err)._0
            var inline207 string = "err " + inline205
            t177 = inline207
        default:
            panic("non-exhaustive match")
        }
        var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
        _goml_runtime_core_string_println(inline199)
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
