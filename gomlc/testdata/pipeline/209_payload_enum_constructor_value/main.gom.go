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

type List__i32 interface {
    isList__i32()
}

type Nil struct {}

func (_ Nil) isList__i32() {}

type Cons struct {
    _0 int32
    _1 List__i32
}

func (_ Cons) isList__i32() {}

func sum(values__3 List__i32) int32 {
    switch values__3.(type) {
    case Nil:
        return 0
    case Cons:
        var x411 int32 = values__3.(Cons)._0
        var x412 List__i32 = values__3.(Cons)._1
        var t421 int32 = sum(x412)
        var t422 int32 = x411 + t421
        return t422
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t424 closure_env_cons_0 = closure_env_cons_0{}
    var cons__6 func(int32, List__i32) List__i32 = func(p0 int32, p1 List__i32) List__i32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(t424, p0, p1)
    }
    var t425 List__i32
    var inline449 int32 = 2
    var inline450 List__i32 = cons__6(inline449, Nil{})
    t425 = inline450
    var values__7 List__i32
    var inline446 int32 = 1
    var inline447 List__i32 = cons__6(inline446, t425)
    values__7 = inline447
    var t426 int32 = sum(values__7)
    var t427 string
    var inline444 string = _goml_runtime_core_int32_to_string(t426)
    t427 = inline444
    var inline441 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline441)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(env413 closure_env_cons_0, ctor_arg_0 int32, ctor_arg_1 List__i32) List__i32 {
    var t439 List__i32 = Cons{
        _0: ctor_arg_0,
        _1: ctor_arg_1,
    }
    return t439
}

func main() {
    main0()
}
