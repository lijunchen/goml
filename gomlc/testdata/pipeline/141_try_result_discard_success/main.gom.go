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
    var mtmp155 Result__string__string = parse_text(ok__1)
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

func show(res__2 Result__string__string) string {
    switch res__2.(type) {
    case Ok:
        var x159 string = res__2.(Ok)._0
        var t179 string = "ok " + x159
        return t179
    case Err:
        var x160 string = res__2.(Err)._0
        var t180 string = "err " + x160
        return t180
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t182 Result__string__string = check(true)
    var t183 string = show(t182)
    println__T_string(t183)
    var t184 Result__string__string = check(false)
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
