package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Result__int32__string interface {
    isResult__int32__string()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

func parse(flag__0 bool) Result__int32__string {
    var retv77 Result__int32__string
    var jp79 Result__int32__string
    if flag__0 {
        var t80 Result__int32__string = Ok{
            _0: 5,
        }
        jp79 = t80
    } else {
        var t81 Result__int32__string = Err{
            _0: "bad-branch",
        }
        jp79 = t81
    }
    retv77 = jp79
    return retv77
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var retv83 Result__int32__string
    var jp85 int32
    if flag__1 {
        var mtmp68 Result__int32__string = parse(fallback__2)
        var jp89 int32
        switch mtmp68.(type) {
        case Ok:
            var x69 int32 = mtmp68.(Ok)._0
            var try_value__13 int32 = x69
            jp89 = try_value__13
            jp85 = jp89
            var value__3 int32 = jp85
            var t86 int32 = value__3 + 1
            var t87 Result__int32__string = Ok{
                _0: t86,
            }
            retv83 = t87
            return retv83
        case Err:
            var x70 string = mtmp68.(Err)._0
            var try_residual__13 string = x70
            var t90 Result__int32__string = Err{
                _0: try_residual__13,
            }
            retv83 = t90
            return retv83
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp85 = 10
        var value__3 int32 = jp85
        var t86 int32 = value__3 + 1
        var t87 Result__int32__string = Ok{
            _0: t86,
        }
        retv83 = t87
        return retv83
    }
}

func show(res__4 Result__int32__string) string {
    var retv92 string
    var jp94 string
    switch res__4.(type) {
    case Ok:
        var x71 int32 = res__4.(Ok)._0
        var value__5 int32 = x71
        var t95 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t96 string = "ok=" + t95
        jp94 = t96
    case Err:
        var x72 string = res__4.(Err)._0
        var err__6 string = x72
        var t97 string = "err=" + err__6
        jp94 = t97
    default:
        panic("non-exhaustive match")
    }
    retv92 = jp94
    return retv92
}

func main0() struct{} {
    var t99 Result__int32__string = bump(true, true)
    var t100 string = show(t99)
    println__T_string(t100)
    var t101 Result__int32__string = bump(true, false)
    var t102 string = show(t101)
    println__T_string(t102)
    var t103 Result__int32__string = bump(false, false)
    var t104 string = show(t103)
    println__T_string(t104)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv106 string
    var t107 string = _goml_runtime_core_int32_to_string(self__6)
    retv106 = t107
    return retv106
}

func println__T_string(value__1 string) struct{} {
    var t109 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t109)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv112 string
    retv112 = self__38
    return retv112
}

func main() {
    main0()
}
