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
        var t169 Result__string__string = Ok{
            _0: "goml",
        }
        return t169
    } else {
        var t170 Result__string__string = Err{
            _0: "parse failed",
        }
        return t170
    }
}

func normalize_text(ok__1 bool) Result__string__string {
    var mtmp155 Result__string__string = parse_text(ok__1)
    var jp174 string
    switch mtmp155.(type) {
    case Ok:
        var x156 string = mtmp155.(Ok)._0
        jp174 = x156
        var t175 string = jp174 + "!"
        var t176 Result__string__string = Ok{
            _0: t175,
        }
        return t176
    case Err:
        var x157 string = mtmp155.(Err)._0
        var t177 Result__string__string = Err{
            _0: x157,
        }
        return t177
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var mtmp158 Result__string__string = normalize_text(ok__3)
    var jp181 string
    switch mtmp158.(type) {
    case Ok:
        var x159 string = mtmp158.(Ok)._0
        jp181 = x159
        var t182 string = "[" + jp181
        var t183 string = t182 + "]"
        var t184 Result__string__string = Ok{
            _0: t183,
        }
        return t184
    case Err:
        var x160 string = mtmp158.(Err)._0
        var t185 Result__string__string = Err{
            _0: x160,
        }
        return t185
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    switch res__5.(type) {
    case Ok:
        var x161 string = res__5.(Ok)._0
        var t190 string = "ok " + x161
        return t190
    case Err:
        var x162 string = res__5.(Err)._0
        var t191 string = "err " + x162
        return t191
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t193 Result__string__string = decorate_text(true)
    var t194 string = show(t193)
    println__T_string(t194)
    var t195 Result__string__string = decorate_text(false)
    var t196 string = show(t195)
    println__T_string(t196)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t198)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
