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
    var retv31 Result__int32__string
    var jp33 Result__int32__string
    if flag__0 {
        var t34 Result__int32__string = Ok{
            _0: 5,
        }
        jp33 = t34
    } else {
        var t35 Result__int32__string = Err{
            _0: "bad-branch",
        }
        jp33 = t35
    }
    retv31 = jp33
    return retv31
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var retv37 Result__int32__string
    var jp39 int32
    if flag__1 {
        var mtmp22 Result__int32__string = parse(fallback__2)
        var jp43 int32
        switch mtmp22.(type) {
        case Ok:
            var x23 int32 = mtmp22.(Ok)._0
            var try_value__13 int32 = x23
            jp43 = try_value__13
            jp39 = jp43
            var value__3 int32 = jp39
            var t40 int32 = value__3 + 1
            var t41 Result__int32__string = Ok{
                _0: t40,
            }
            retv37 = t41
            return retv37
        case Err:
            var x24 string = mtmp22.(Err)._0
            var try_residual__13 string = x24
            var t44 Result__int32__string = Err{
                _0: try_residual__13,
            }
            retv37 = t44
            return retv37
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp39 = 10
        var value__3 int32 = jp39
        var t40 int32 = value__3 + 1
        var t41 Result__int32__string = Ok{
            _0: t40,
        }
        retv37 = t41
        return retv37
    }
}

func show(res__4 Result__int32__string) string {
    var retv46 string
    var jp48 string
    switch res__4.(type) {
    case Ok:
        var x25 int32 = res__4.(Ok)._0
        var value__5 int32 = x25
        var t49 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t50 string = "ok=" + t49
        jp48 = t50
    case Err:
        var x26 string = res__4.(Err)._0
        var err__6 string = x26
        var t51 string = "err=" + err__6
        jp48 = t51
    default:
        panic("non-exhaustive match")
    }
    retv46 = jp48
    return retv46
}

func main0() struct{} {
    var t53 Result__int32__string = bump(true, true)
    var t54 string = show(t53)
    println__T_string(t54)
    var t55 Result__int32__string = bump(true, false)
    var t56 string = show(t55)
    println__T_string(t56)
    var t57 Result__int32__string = bump(false, false)
    var t58 string = show(t57)
    println__T_string(t58)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv60 string
    var t61 string = _goml_runtime_core_int32_to_string(self__2)
    retv60 = t61
    return retv60
}

func println__T_string(value__1 string) struct{} {
    var t63 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t63)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv66 string
    retv66 = self__9
    return retv66
}

func main() {
    main0()
}
