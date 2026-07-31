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

type Ok struct {
    _0 struct{}
}

func (_ Ok) isResult__unit__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__unit__string() {}

func step(ok__0 bool) Result__unit__string {
    var retv161 Result__unit__string
    var jp163 Result__unit__string
    if ok__0 {
        var t164 Result__unit__string = Ok{
            _0: struct{}{},
        }
        jp163 = t164
    } else {
        var t165 Result__unit__string = Err{
            _0: "step failed",
        }
        jp163 = t165
    }
    retv161 = jp163
    return retv161
}

func run(ok__1 bool) Result__unit__string {
    var retv167 Result__unit__string
    var mtmp152 Result__unit__string = step(ok__1)
    switch mtmp152.(type) {
    case Ok:
        var t169 Result__unit__string = Ok{
            _0: struct{}{},
        }
        retv167 = t169
        return retv167
    case Err:
        var x154 string = mtmp152.(Err)._0
        var try_residual__12 string = x154
        var t170 Result__unit__string = Err{
            _0: try_residual__12,
        }
        retv167 = t170
        return retv167
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__unit__string) string {
    var retv172 string
    var jp174 string
    switch res__2.(type) {
    case Ok:
        var jp176 string
        jp176 = "ok unit"
        jp174 = jp176
    case Err:
        var x157 string = res__2.(Err)._0
        var err__3 string = x157
        var t177 string = "err " + err__3
        jp174 = t177
    default:
        panic("non-exhaustive match")
    }
    retv172 = jp174
    return retv172
}

func main0() struct{} {
    var t179 Result__unit__string = run(true)
    var t180 string = show(t179)
    println__T_string(t180)
    var t181 Result__unit__string = run(false)
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
