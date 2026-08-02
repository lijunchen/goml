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
        var t167 Result__string__string = Ok{
            _0: "ignored",
        }
        return t167
    } else {
        var t168 Result__string__string = Err{
            _0: "parse failed",
        }
        return t168
    }
}

func check(ok__1 bool) Result__string__string {
    var mtmp155 Result__string__string
    if ok__1 {
        var inline192 Result__string__string = Ok{
            _0: "ignored",
        }
        mtmp155 = inline192
    } else {
        var inline193 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp155 = inline193
    }
    switch mtmp155.(type) {
    case Ok:
        var t173 Result__string__string = Ok{
            _0: "ok",
        }
        return t173
    case Err:
        var x157 string = mtmp155.(Err)._0
        var t174 Result__string__string = Err{
            _0: x157,
        }
        return t174
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t182 Result__string__string = check(true)
    var t183 string
    switch t182.(type) {
    case Ok:
        var inline220 string = t182.(Ok)._0
        var inline222 string = "ok " + inline220
        t183 = inline222
    case Err:
        var inline223 string = t182.(Err)._0
        var inline225 string = "err " + inline223
        t183 = inline225
    default:
        panic("non-exhaustive match")
    }
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline217)
    var t184 Result__string__string
    var inline205 bool = false
    var inline206 Result__string__string = parse_text(inline205)
    switch inline206.(type) {
    case Ok:
        var inline210 Result__string__string = Ok{
            _0: "ok",
        }
        t184 = inline210
        var t185 string
        switch t184.(type) {
        case Ok:
            var inline198 string = t184.(Ok)._0
            var inline200 string = "ok " + inline198
            t185 = inline200
        case Err:
            var inline201 string = t184.(Err)._0
            var inline203 string = "err " + inline201
            t185 = inline203
        default:
            panic("non-exhaustive match")
        }
        var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
        _goml_runtime_core_string_println(inline195)
        return struct{}{}
    case Err:
        var inline213 string = inline206.(Err)._0
        var inline215 Result__string__string = Err{
            _0: inline213,
        }
        t184 = inline215
        var t185 string
        switch t184.(type) {
        case Ok:
            var inline198 string = t184.(Ok)._0
            var inline200 string = "ok " + inline198
            t185 = inline200
        case Err:
            var inline201 string = t184.(Err)._0
            var inline203 string = "err " + inline201
            t185 = inline203
        default:
            panic("non-exhaustive match")
        }
        var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
        _goml_runtime_core_string_println(inline195)
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
