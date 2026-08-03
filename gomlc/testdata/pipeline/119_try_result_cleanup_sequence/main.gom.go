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
    var mtmp136 Result__Handle__string
    if open_ok__3 {
        var inline196 Handle = Handle{
            name: "config",
        }
        var inline197 Result__Handle__string = Result__Handle__string_Ok{
            _0: inline196,
        }
        mtmp136 = inline197
    } else {
        var inline198 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        mtmp136 = inline198
    }
    var jp166 Handle
    switch mtmp136.(type) {
    case Result__Handle__string_Ok:
        var x137 Handle = mtmp136.(Result__Handle__string_Ok)._0
        jp166 = x137
        var name__6 string = jp166.name
        var mtmp139 Result__unit__string
        if close_ok__4 {
            var inline191 Result__unit__string = Result__unit__string_Ok{
                _0: struct{}{},
            }
            mtmp139 = inline191
        } else {
            var inline192 string = jp166.name
            var inline193 string = "close failed for " + inline192
            var inline194 Result__unit__string = Result__unit__string_Err{
                _0: inline193,
            }
            mtmp139 = inline194
        }
        switch mtmp139.(type) {
        case Result__unit__string_Ok:
            var t168 string = "closed " + name__6
            var t169 Result__string__string = Result__string__string_Ok{
                _0: t168,
            }
            return t169
        case Result__unit__string_Err:
            var x141 string = mtmp139.(Result__unit__string_Err)._0
            var t170 Result__string__string = Result__string__string_Err{
                _0: x141,
            }
            return t170
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x138 string = mtmp136.(Result__Handle__string_Err)._0
        var t171 Result__string__string = Result__string__string_Err{
            _0: x138,
        }
        return t171
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t179 Result__string__string = use_handle(true, true)
    var t180 string
    switch t179.(type) {
    case Result__string__string_Ok:
        var inline223 string = t179.(Result__string__string_Ok)._0
        var inline225 string = "ok " + inline223
        t180 = inline225
    case Result__string__string_Err:
        var inline226 string = t179.(Result__string__string_Err)._0
        var inline228 string = "err " + inline226
        t180 = inline228
    default:
        panic("non-exhaustive match")
    }
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline220)
    var t181 Result__string__string = use_handle(false, true)
    var t182 string
    switch t181.(type) {
    case Result__string__string_Ok:
        var inline213 string = t181.(Result__string__string_Ok)._0
        var inline215 string = "ok " + inline213
        t182 = inline215
    case Result__string__string_Err:
        var inline216 string = t181.(Result__string__string_Err)._0
        var inline218 string = "err " + inline216
        t182 = inline218
    default:
        panic("non-exhaustive match")
    }
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline210)
    var t183 Result__string__string = use_handle(true, false)
    var t184 string
    switch t183.(type) {
    case Result__string__string_Ok:
        var inline203 string = t183.(Result__string__string_Ok)._0
        var inline205 string = "ok " + inline203
        t184 = inline205
    case Result__string__string_Err:
        var inline206 string = t183.(Result__string__string_Err)._0
        var inline208 string = "err " + inline206
        t184 = inline208
    default:
        panic("non-exhaustive match")
    }
    var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline200)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
