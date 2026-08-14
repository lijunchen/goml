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

type closure_env_cons_0 struct {}

type Ordering int32

type List__int32 interface {
    isList__int32()
}

type Nil struct {}

func (_ Nil) isList__int32() {}

type Cons struct {
    _0 int32
    _1 List__int32
}

func (_ Cons) isList__int32() {}

func sum(values__3 List__int32) int32 {
    switch values__3.(type) {
    case Nil:
        return 0
    case Cons:
        var x408 int32 = values__3.(Cons)._0
        var x409 List__int32 = values__3.(Cons)._1
        var t418 int32 = sum(x409)
        var t419 int32 = x408 + t418
        return t419
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t421 closure_env_cons_0 = closure_env_cons_0{}
    var cons__6 func(int32, List__int32) List__int32 = func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(t421, p0, p1)
    }
    var t422 List__int32
    var inline446 int32 = 2
    var inline447 List__int32 = cons__6(inline446, Nil{})
    t422 = inline447
    var values__7 List__int32
    var inline443 int32 = 1
    var inline444 List__int32 = cons__6(inline443, t422)
    values__7 = inline444
    var t423 int32 = sum(values__7)
    var t424 string
    var inline441 string = _goml_runtime_core_int32_to_string(t423)
    t424 = inline441
    var inline438 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline438)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(env410 closure_env_cons_0, ctor_arg_0 int32, ctor_arg_1 List__int32) List__int32 {
    var t436 List__int32 = Cons{
        _0: ctor_arg_0,
        _1: ctor_arg_1,
    }
    return t436
}

func main() {
    main0()
}
