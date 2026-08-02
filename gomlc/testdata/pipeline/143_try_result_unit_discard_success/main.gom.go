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
    if ok__0 {
        var t167 Result__unit__string = Ok{
            _0: struct{}{},
        }
        return t167
    } else {
        var t168 Result__unit__string = Err{
            _0: "step failed",
        }
        return t168
    }
}

func run(ok__1 bool) Result__unit__string {
    var mtmp155 Result__unit__string = step(ok__1)
    switch mtmp155.(type) {
    case Ok:
        var t172 Result__unit__string = Ok{
            _0: struct{}{},
        }
        return t172
    case Err:
        var x157 string = mtmp155.(Err)._0
        var t173 Result__unit__string = Err{
            _0: x157,
        }
        return t173
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__unit__string) string {
    switch res__2.(type) {
    case Ok:
        return "ok unit"
    case Err:
        var x160 string = res__2.(Err)._0
        var t180 string = "err " + x160
        return t180
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t182 Result__unit__string = run(true)
    var t183 string = show(t182)
    println__T_string(t183)
    var t184 Result__unit__string = run(false)
    var t185 string = show(t184)
    println__T_string(t185)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
