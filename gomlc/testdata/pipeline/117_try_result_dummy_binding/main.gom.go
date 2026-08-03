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
    var mtmp136 Result__unit__string
    if config_ok__3 {
        var inline196 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        mtmp136 = inline196
    } else {
        var inline197 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        mtmp136 = inline197
    }
    switch mtmp136.(type) {
    case Result__unit__string_Ok:
        var mtmp140 Result__string__string
        if read_ok__4 {
            var inline193 Result__string__string = Result__string__string_Ok{
                _0: "2s",
            }
            mtmp140 = inline193
        } else {
            var inline194 Result__string__string = Result__string__string_Err{
                _0: "duration failed",
            }
            mtmp140 = inline194
        }
        var jp167 string
        switch mtmp140.(type) {
        case Result__string__string_Ok:
            var x141 string = mtmp140.(Result__string__string_Ok)._0
            jp167 = x141
            var t168 string
            var inline191 string = "duration=" + jp167
            t168 = inline191
            var t169 Result__string__string = Result__string__string_Ok{
                _0: t168,
            }
            return t169
        case Result__string__string_Err:
            var x142 string = mtmp140.(Result__string__string_Err)._0
            var t170 Result__string__string = Result__string__string_Err{
                _0: x142,
            }
            return t170
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x138 string = mtmp136.(Result__unit__string_Err)._0
        var t171 Result__string__string = Result__string__string_Err{
            _0: x138,
        }
        return t171
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t179 Result__string__string = configure_and_format(true, true)
    var t180 string
    switch t179.(type) {
    case Result__string__string_Ok:
        var inline222 string = t179.(Result__string__string_Ok)._0
        var inline224 string = "ok " + inline222
        t180 = inline224
    case Result__string__string_Err:
        var inline225 string = t179.(Result__string__string_Err)._0
        var inline227 string = "err " + inline225
        t180 = inline227
    default:
        panic("non-exhaustive match")
    }
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline219)
    var t181 Result__string__string = configure_and_format(true, false)
    var t182 string
    switch t181.(type) {
    case Result__string__string_Ok:
        var inline212 string = t181.(Result__string__string_Ok)._0
        var inline214 string = "ok " + inline212
        t182 = inline214
    case Result__string__string_Err:
        var inline215 string = t181.(Result__string__string_Err)._0
        var inline217 string = "err " + inline215
        t182 = inline217
    default:
        panic("non-exhaustive match")
    }
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline209)
    var t183 Result__string__string = configure_and_format(false, true)
    var t184 string
    switch t183.(type) {
    case Result__string__string_Ok:
        var inline202 string = t183.(Result__string__string_Ok)._0
        var inline204 string = "ok " + inline202
        t184 = inline204
    case Result__string__string_Err:
        var inline205 string = t183.(Result__string__string_Err)._0
        var inline207 string = "err " + inline205
        t184 = inline207
    default:
        panic("non-exhaustive match")
    }
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
