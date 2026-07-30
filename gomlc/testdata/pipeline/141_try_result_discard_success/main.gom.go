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
    var retv77 Result__string__string
    var jp79 Result__string__string
    if ok__0 {
        var t80 Result__string__string = Ok{
            _0: "ignored",
        }
        jp79 = t80
    } else {
        var t81 Result__string__string = Err{
            _0: "parse failed",
        }
        jp79 = t81
    }
    retv77 = jp79
    return retv77
}

func check(ok__1 bool) Result__string__string {
    var retv83 Result__string__string
    var mtmp68 Result__string__string = parse_text(ok__1)
    switch mtmp68.(type) {
    case Ok:
        var t86 Result__string__string = Ok{
            _0: "ok",
        }
        retv83 = t86
        return retv83
    case Err:
        var x70 string = mtmp68.(Err)._0
        var try_residual__12 string = x70
        var t87 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv83 = t87
        return retv83
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__string__string) string {
    var retv89 string
    var jp91 string
    switch res__2.(type) {
    case Ok:
        var x72 string = res__2.(Ok)._0
        var value__3 string = x72
        var t92 string = "ok " + value__3
        jp91 = t92
    case Err:
        var x73 string = res__2.(Err)._0
        var err__4 string = x73
        var t93 string = "err " + err__4
        jp91 = t93
    default:
        panic("non-exhaustive match")
    }
    retv89 = jp91
    return retv89
}

func main0() struct{} {
    var t95 Result__string__string = check(true)
    var t96 string = show(t95)
    println__T_string(t96)
    var t97 Result__string__string = check(false)
    var t98 string = show(t97)
    println__T_string(t98)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv103 string
    retv103 = self__38
    return retv103
}

func main() {
    main0()
}
