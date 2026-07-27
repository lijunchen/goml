package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Handle struct {
    name string
}

type Result__Handle__string interface {
    isResult__Handle__string()
}

type Result__Handle__string_Ok struct {
    _0 Handle
}

func (_ Result__Handle__string_Ok) isResult__Handle__string() {}

type Result__Handle__string_Err struct {
    _0 string
}

func (_ Result__Handle__string_Err) isResult__Handle__string() {}

type Result__unit__string interface {
    isResult__unit__string()
}

type Result__unit__string_Ok struct {
    _0 struct{}
}

func (_ Result__unit__string_Ok) isResult__unit__string() {}

type Result__unit__string_Err struct {
    _0 string
}

func (_ Result__unit__string_Err) isResult__unit__string() {}

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

func open_handle(ok__0 bool) Result__Handle__string {
    var retv77 Result__Handle__string
    var jp79 Result__Handle__string
    if ok__0 {
        var t80 Handle = Handle{
            name: "config",
        }
        var t81 Result__Handle__string = Result__Handle__string_Ok{
            _0: t80,
        }
        jp79 = t81
    } else {
        var t82 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        jp79 = t82
    }
    retv77 = jp79
    return retv77
}

func close_handle(handle__1 Handle, ok__2 bool) Result__unit__string {
    var retv84 Result__unit__string
    var jp86 Result__unit__string
    if ok__2 {
        var t87 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp86 = t87
    } else {
        var t88 string = handle__1.name
        var t89 string = "close failed for " + t88
        var t90 Result__unit__string = Result__unit__string_Err{
            _0: t89,
        }
        jp86 = t90
    }
    retv84 = jp86
    return retv84
}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var retv92 Result__string__string
    var mtmp64 Result__Handle__string = open_handle(open_ok__3)
    var jp94 Handle
    switch mtmp64.(type) {
    case Result__Handle__string_Ok:
        var x65 Handle = mtmp64.(Result__Handle__string_Ok)._0
        var try_value__24 Handle = x65
        jp94 = try_value__24
        var handle__5 Handle = jp94
        var name__6 string = handle__5.name
        var mtmp67 Result__unit__string = close_handle(handle__5, close_ok__4)
        switch mtmp67.(type) {
        case Result__unit__string_Ok:
            var t96 string = "closed " + name__6
            var t97 Result__string__string = Result__string__string_Ok{
                _0: t96,
            }
            retv92 = t97
            return retv92
        case Result__unit__string_Err:
            var x69 string = mtmp67.(Result__unit__string_Err)._0
            var try_residual__31 string = x69
            var t98 Result__string__string = Result__string__string_Err{
                _0: try_residual__31,
            }
            retv92 = t98
            return retv92
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x66 string = mtmp64.(Result__Handle__string_Err)._0
        var try_residual__24 string = x66
        var t99 Result__string__string = Result__string__string_Err{
            _0: try_residual__24,
        }
        retv92 = t99
        return retv92
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__string__string) string {
    var retv101 string
    var jp103 string
    switch res__7.(type) {
    case Result__string__string_Ok:
        var x71 string = res__7.(Result__string__string_Ok)._0
        var value__8 string = x71
        var t104 string = "ok " + value__8
        jp103 = t104
    case Result__string__string_Err:
        var x72 string = res__7.(Result__string__string_Err)._0
        var err__9 string = x72
        var t105 string = "err " + err__9
        jp103 = t105
    default:
        panic("non-exhaustive match")
    }
    retv101 = jp103
    return retv101
}

func main0() struct{} {
    var t107 Result__string__string = use_handle(true, true)
    var t108 string = show(t107)
    println__T_string(t108)
    var t109 Result__string__string = use_handle(false, true)
    var t110 string = show(t109)
    println__T_string(t110)
    var t111 Result__string__string = use_handle(true, false)
    var t112 string = show(t111)
    println__T_string(t112)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t114 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t114)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv117 string
    retv117 = self__38
    return retv117
}

func main() {
    main0()
}
