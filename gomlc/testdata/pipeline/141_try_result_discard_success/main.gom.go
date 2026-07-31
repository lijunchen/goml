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
    var retv161 Result__string__string
    var jp163 Result__string__string
    if ok__0 {
        var t164 Result__string__string = Ok{
            _0: "ignored",
        }
        jp163 = t164
    } else {
        var t165 Result__string__string = Err{
            _0: "parse failed",
        }
        jp163 = t165
    }
    retv161 = jp163
    return retv161
}

func check(ok__1 bool) Result__string__string {
    var retv167 Result__string__string
    var mtmp152 Result__string__string = parse_text(ok__1)
    switch mtmp152.(type) {
    case Ok:
        var t170 Result__string__string = Ok{
            _0: "ok",
        }
        retv167 = t170
        return retv167
    case Err:
        var x154 string = mtmp152.(Err)._0
        var try_residual__12 string = x154
        var t171 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv167 = t171
        return retv167
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__string__string) string {
    var retv173 string
    var jp175 string
    switch res__2.(type) {
    case Ok:
        var x156 string = res__2.(Ok)._0
        var value__3 string = x156
        var t176 string = "ok " + value__3
        jp175 = t176
    case Err:
        var x157 string = res__2.(Err)._0
        var err__4 string = x157
        var t177 string = "err " + err__4
        jp175 = t177
    default:
        panic("non-exhaustive match")
    }
    retv173 = jp175
    return retv173
}

func main0() struct{} {
    var t179 Result__string__string = check(true)
    var t180 string = show(t179)
    println__T_string(t180)
    var t181 Result__string__string = check(false)
    var t182 string = show(t181)
    println__T_string(t182)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t184)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv187 string
    retv187 = self__38
    return retv187
}

func main() {
    main0()
}
