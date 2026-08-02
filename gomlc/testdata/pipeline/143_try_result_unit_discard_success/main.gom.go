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
    var retv164 Result__unit__string
    var jp166 Result__unit__string
    if ok__0 {
        var t167 Result__unit__string = Ok{
            _0: struct{}{},
        }
        jp166 = t167
    } else {
        var t168 Result__unit__string = Err{
            _0: "step failed",
        }
        jp166 = t168
    }
    retv164 = jp166
    return retv164
}

func run(ok__1 bool) Result__unit__string {
    var retv170 Result__unit__string
    var mtmp155 Result__unit__string = step(ok__1)
    switch mtmp155.(type) {
    case Ok:
        var t172 Result__unit__string = Ok{
            _0: struct{}{},
        }
        retv170 = t172
        return retv170
    case Err:
        var x157 string = mtmp155.(Err)._0
        var try_residual__12 string = x157
        var t173 Result__unit__string = Err{
            _0: try_residual__12,
        }
        retv170 = t173
        return retv170
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__unit__string) string {
    var retv175 string
    var jp177 string
    switch res__2.(type) {
    case Ok:
        var jp179 string
        jp179 = "ok unit"
        jp177 = jp179
    case Err:
        var x160 string = res__2.(Err)._0
        var err__3 string = x160
        var t180 string = "err " + err__3
        jp177 = t180
    default:
        panic("non-exhaustive match")
    }
    retv175 = jp177
    return retv175
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
    var retv190 string
    retv190 = self__38
    return retv190
}

func main() {
    main0()
}
