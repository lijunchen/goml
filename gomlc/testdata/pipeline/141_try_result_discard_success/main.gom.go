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
    var retv117 Result__string__string
    var jp119 Result__string__string
    if ok__0 {
        var t120 Result__string__string = Ok{
            _0: "ignored",
        }
        jp119 = t120
    } else {
        var t121 Result__string__string = Err{
            _0: "parse failed",
        }
        jp119 = t121
    }
    retv117 = jp119
    return retv117
}

func check(ok__1 bool) Result__string__string {
    var retv123 Result__string__string
    var mtmp108 Result__string__string = parse_text(ok__1)
    switch mtmp108.(type) {
    case Ok:
        var t126 Result__string__string = Ok{
            _0: "ok",
        }
        retv123 = t126
        return retv123
    case Err:
        var x110 string = mtmp108.(Err)._0
        var try_residual__12 string = x110
        var t127 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv123 = t127
        return retv123
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__string__string) string {
    var retv129 string
    var jp131 string
    switch res__2.(type) {
    case Ok:
        var x112 string = res__2.(Ok)._0
        var value__3 string = x112
        var t132 string = "ok " + value__3
        jp131 = t132
    case Err:
        var x113 string = res__2.(Err)._0
        var err__4 string = x113
        var t133 string = "err " + err__4
        jp131 = t133
    default:
        panic("non-exhaustive match")
    }
    retv129 = jp131
    return retv129
}

func main0() struct{} {
    var t135 Result__string__string = check(true)
    var t136 string = show(t135)
    println__T_string(t136)
    var t137 Result__string__string = check(false)
    var t138 string = show(t137)
    println__T_string(t138)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t140 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t140)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv143 string
    retv143 = self__38
    return retv143
}

func main() {
    main0()
}
