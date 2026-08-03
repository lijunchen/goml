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
        var t148 Result__string__string = Ok{
            _0: "ignored",
        }
        return t148
    } else {
        var t149 Result__string__string = Err{
            _0: "parse failed",
        }
        return t149
    }
}

func check(ok__1 bool) Result__string__string {
    var mtmp136 Result__string__string
    if ok__1 {
        var inline173 Result__string__string = Ok{
            _0: "ignored",
        }
        mtmp136 = inline173
    } else {
        var inline174 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp136 = inline174
    }
    switch mtmp136.(type) {
    case Ok:
        var t154 Result__string__string = Ok{
            _0: "ok",
        }
        return t154
    case Err:
        var x138 string = mtmp136.(Err)._0
        var t155 Result__string__string = Err{
            _0: x138,
        }
        return t155
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t163 Result__string__string = check(true)
    var t164 string
    switch t163.(type) {
    case Ok:
        var inline201 string = t163.(Ok)._0
        var inline203 string = "ok " + inline201
        t164 = inline203
    case Err:
        var inline204 string = t163.(Err)._0
        var inline206 string = "err " + inline204
        t164 = inline206
    default:
        panic("non-exhaustive match")
    }
    var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
    _goml_runtime_core_string_println(inline198)
    var t165 Result__string__string
    var inline186 bool = false
    var inline187 Result__string__string = parse_text(inline186)
    switch inline187.(type) {
    case Ok:
        var inline191 Result__string__string = Ok{
            _0: "ok",
        }
        t165 = inline191
        var t166 string
        switch t165.(type) {
        case Ok:
            var inline179 string = t165.(Ok)._0
            var inline181 string = "ok " + inline179
            t166 = inline181
        case Err:
            var inline182 string = t165.(Err)._0
            var inline184 string = "err " + inline182
            t166 = inline184
        default:
            panic("non-exhaustive match")
        }
        var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
        _goml_runtime_core_string_println(inline176)
        return struct{}{}
    case Err:
        var inline194 string = inline187.(Err)._0
        var inline196 Result__string__string = Err{
            _0: inline194,
        }
        t165 = inline196
        var t166 string
        switch t165.(type) {
        case Ok:
            var inline179 string = t165.(Ok)._0
            var inline181 string = "ok " + inline179
            t166 = inline181
        case Err:
            var inline182 string = t165.(Err)._0
            var inline184 string = "err " + inline182
            t166 = inline184
        default:
            panic("non-exhaustive match")
        }
        var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
        _goml_runtime_core_string_println(inline176)
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
