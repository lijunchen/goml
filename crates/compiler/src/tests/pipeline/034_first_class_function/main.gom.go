package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_closure_apply_0 struct {}

type closure_env_global_invoker_1 struct {}

type closure_env_composer_closure_2 struct {}

type GoError = error

func double(x__0 int32) int32 {
    var retv8 int32
    var t9 int32 = x__0 * 2
    retv8 = t9
    return retv8
}

func increment(x__1 int32) int32 {
    var retv11 int32
    var t12 int32 = x__1 + 1
    retv11 = t12
    return retv11
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var retv14 int32
    var t15 int32 = f__2(value__3)
    retv14 = t15
    return retv14
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var retv17 int32
    var t18 int32 = g__5(value__6)
    var t19 int32 = f__4(t18)
    retv17 = t19
    return retv17
}

func main0() struct{} {
    var local__7 func(int32) int32 = double
    var first__8 int32 = apply_once(local__7, 4)
    var composed__9 int32 = compose(double, increment, first__8)
    var closure_apply__11 closure_env_closure_apply_0 = closure_env_closure_apply_0{}
    var closure_result__12 int32 = _goml_inherent_x23_closure_x5f_env_x5f_closure_x5f_apply_x5f_0_x23_closure_x5f_env_x5f_closure_x5f_apply_x5f_0_x23_apply(closure_apply__11, composed__9)
    var global_invoker__15 closure_env_global_invoker_1 = closure_env_global_invoker_1{}
    var invoked_with_global__16 int32 = _goml_inherent_x23_closure_x5f_env_x5f_global_x5f_invoker_x5f_1_x23_closure_x5f_env_x5f_global_x5f_invoker_x5f_1_x23_apply(global_invoker__15, double, 3)
    var composer_closure__18 closure_env_composer_closure_2 = closure_env_composer_closure_2{}
    var composed_by_closure__19 int32 = _goml_inherent_x23_closure_x5f_env_x5f_composer_x5f_closure_x5f_2_x23_closure_x5f_env_x5f_composer_x5f_closure_x5f_2_x23_apply(composer_closure__18, 5)
    var printer__20 func(string) struct{} = string_println
    var t21 string = int32_to_string(composed__9)
    printer__20(t21)
    var t22 string = int32_to_string(closure_result__12)
    printer__20(t22)
    var t23 string = int32_to_string(invoked_with_global__16)
    printer__20(t23)
    var t24 string = int32_to_string(composed_by_closure__19)
    printer__20(t24)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_closure_x5f_apply_x5f_0_x23_closure_x5f_env_x5f_closure_x5f_apply_x5f_0_x23_apply(env4 closure_env_closure_apply_0, value__10 int32) int32 {
    var retv26 int32
    var t27 int32 = apply_once(increment, value__10)
    retv26 = t27
    return retv26
}

func _goml_inherent_x23_closure_x5f_env_x5f_global_x5f_invoker_x5f_1_x23_closure_x5f_env_x5f_global_x5f_invoker_x5f_1_x23_apply(env5 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var retv29 int32
    var t30 int32 = apply_once(func_to_call__13, value__14)
    retv29 = t30
    return retv29
}

func _goml_inherent_x23_closure_x5f_env_x5f_composer_x5f_closure_x5f_2_x23_closure_x5f_env_x5f_composer_x5f_closure_x5f_2_x23_apply(env6 closure_env_composer_closure_2, value__17 int32) int32 {
    var retv32 int32
    var t33 int32 = compose(double, increment, value__17)
    retv32 = t33
    return retv32
}

func main() {
    main0()
}
