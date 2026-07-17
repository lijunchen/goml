package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type _goml_m_Result_____o_string_c_string_q_____string interface {
    is_goml_m_Result_____o_string_c_string_q_____string()
}

type _goml_m_Result_____o_string_c_string_q_____string_Ok struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Result_____o_string_c_string_q_____string_Ok) is_goml_m_Result_____o_string_c_string_q_____string() {}

type _goml_m_Result_____o_string_c_string_q_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result_____o_string_c_string_q_____string_Err) is_goml_m_Result_____o_string_c_string_q_____string() {}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

func split_host_port(ok__0 bool) _goml_m_Result_____o_string_c_string_q_____string {
    var retv69 _goml_m_Result_____o_string_c_string_q_____string
    var jp71 _goml_m_Result_____o_string_c_string_q_____string
    if ok__0 {
        var t72 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var t73 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t72,
        }
        jp71 = t73
    } else {
        var t74 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        jp71 = t74
    }
    retv69 = jp71
    return retv69
}

func render(ok__1 bool) Result__string__string {
    var retv76 Result__string__string
    var mtmp58 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    var jp78 Tuple2_6string_6string
    switch mtmp58.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x59 Tuple2_6string_6string = mtmp58.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__14 Tuple2_6string_6string = x59
        jp78 = try_value__14
        var mtmp61 Tuple2_6string_6string = jp78
        var x62 string = mtmp61._0
        var x63 string = mtmp61._1
        var port__3 string = x63
        var host__2 string = x62
        var t79 string = host__2 + ":"
        var t80 string = t79 + port__3
        var t81 Result__string__string = Result__string__string_Ok{
            _0: t80,
        }
        retv76 = t81
        return retv76
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x60 string = mtmp58.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__14 string = x60
        var t82 Result__string__string = Result__string__string_Err{
            _0: try_residual__14,
        }
        retv76 = t82
        return retv76
    default:
        panic("non-exhaustive match")
    }
}

func show(res__4 Result__string__string) string {
    var retv84 string
    var jp86 string
    switch res__4.(type) {
    case Result__string__string_Ok:
        var x64 string = res__4.(Result__string__string_Ok)._0
        var value__5 string = x64
        var t87 string = "ok " + value__5
        jp86 = t87
    case Result__string__string_Err:
        var x65 string = res__4.(Result__string__string_Err)._0
        var err__6 string = x65
        var t88 string = "err " + err__6
        jp86 = t88
    default:
        panic("non-exhaustive match")
    }
    retv84 = jp86
    return retv84
}

func main0() struct{} {
    var t90 Result__string__string = render(true)
    var t91 string = show(t90)
    println__T_string(t91)
    var t92 Result__string__string = render(false)
    var t93 string = show(t92)
    println__T_string(t93)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv98 string
    retv98 = self__34
    return retv98
}

func main() {
    main0()
}
