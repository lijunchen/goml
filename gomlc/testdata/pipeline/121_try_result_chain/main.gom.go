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
    var retv119 Result__string__string
    var jp121 Result__string__string
    if ok__0 {
        var t122 Result__string__string = Ok{
            _0: "goml",
        }
        jp121 = t122
    } else {
        var t123 Result__string__string = Err{
            _0: "parse failed",
        }
        jp121 = t123
    }
    retv119 = jp121
    return retv119
}

func normalize_text(ok__1 bool) Result__string__string {
    var retv125 Result__string__string
    var mtmp108 Result__string__string = parse_text(ok__1)
    var jp127 string
    switch mtmp108.(type) {
    case Ok:
        var x109 string = mtmp108.(Ok)._0
        var try_value__12 string = x109
        jp127 = try_value__12
        var text__2 string = jp127
        var t128 string = text__2 + "!"
        var t129 Result__string__string = Ok{
            _0: t128,
        }
        retv125 = t129
        return retv125
    case Err:
        var x110 string = mtmp108.(Err)._0
        var try_residual__12 string = x110
        var t130 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv125 = t130
        return retv125
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var retv132 Result__string__string
    var mtmp111 Result__string__string = normalize_text(ok__3)
    var jp134 string
    switch mtmp111.(type) {
    case Ok:
        var x112 string = mtmp111.(Ok)._0
        var try_value__20 string = x112
        jp134 = try_value__20
        var text__4 string = jp134
        var t135 string = "[" + text__4
        var t136 string = t135 + "]"
        var t137 Result__string__string = Ok{
            _0: t136,
        }
        retv132 = t137
        return retv132
    case Err:
        var x113 string = mtmp111.(Err)._0
        var try_residual__20 string = x113
        var t138 Result__string__string = Err{
            _0: try_residual__20,
        }
        retv132 = t138
        return retv132
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv140 string
    var jp142 string
    switch res__5.(type) {
    case Ok:
        var x114 string = res__5.(Ok)._0
        var value__6 string = x114
        var t143 string = "ok " + value__6
        jp142 = t143
    case Err:
        var x115 string = res__5.(Err)._0
        var err__7 string = x115
        var t144 string = "err " + err__7
        jp142 = t144
    default:
        panic("non-exhaustive match")
    }
    retv140 = jp142
    return retv140
}

func main0() struct{} {
    var t146 Result__string__string = decorate_text(true)
    var t147 string = show(t146)
    println__T_string(t147)
    var t148 Result__string__string = decorate_text(false)
    var t149 string = show(t148)
    println__T_string(t149)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t151 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t151)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv154 string
    retv154 = self__38
    return retv154
}

func main() {
    main0()
}
