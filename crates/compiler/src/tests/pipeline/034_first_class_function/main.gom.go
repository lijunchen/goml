package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type closure_env_closure_apply_0 struct {}

type closure_env_global_invoker_1 struct {}

type closure_env_composer_closure_2 struct {}

func double(x__0 int32) int32 {
    var ret23 int32
    ret23 = x__0 * 2
    return ret23
}

func increment(x__1 int32) int32 {
    var ret24 int32
    ret24 = x__1 + 1
    return ret24
}

func apply_once(f__2 func(int32) int32, value__3 int32) int32 {
    var ret25 int32
    ret25 = f__2(value__3)
    return ret25
}

func compose(f__4 func(int32) int32, g__5 func(int32) int32, value__6 int32) int32 {
    var ret26 int32
    var t18 int32 = g__5(value__6)
    ret26 = f__4(t18)
    return ret26
}

func main0() struct{} {
    var ret27 struct{}
    var local__7 func(int32) int32 = double
    var first__8 int32 = apply_once(local__7, 4)
    var composed__9 int32 = compose(double, increment, first__8)
    var closure_apply__11 closure_env_closure_apply_0 = closure_env_closure_apply_0{}
    var closure_result__12 int32 = _goml_inherent_closure_env_closure_apply_0_closure_env_closure_apply_0_apply(closure_apply__11, composed__9)
    var global_invoker__15 closure_env_global_invoker_1 = closure_env_global_invoker_1{}
    var invoked_with_global__16 int32 = _goml_inherent_closure_env_global_invoker_1_closure_env_global_invoker_1_apply(global_invoker__15, double, 3)
    var composer_closure__18 closure_env_composer_closure_2 = closure_env_composer_closure_2{}
    var composed_by_closure__19 int32 = _goml_inherent_closure_env_composer_closure_2_closure_env_composer_closure_2_apply(composer_closure__18, 5)
    var printer__20 func(string) struct{} = string_println
    var t19 string = int32_to_string(composed__9)
    printer__20(t19)
    var t20 string = int32_to_string(closure_result__12)
    printer__20(t20)
    var t21 string = int32_to_string(invoked_with_global__16)
    printer__20(t21)
    var t22 string = int32_to_string(composed_by_closure__19)
    printer__20(t22)
    ret27 = struct{}{}
    return ret27
}

func _goml_inherent_closure_env_closure_apply_0_closure_env_closure_apply_0_apply(env15 closure_env_closure_apply_0, value__10 int32) int32 {
    var ret28 int32
    ret28 = apply_once(increment, value__10)
    return ret28
}

func _goml_inherent_closure_env_global_invoker_1_closure_env_global_invoker_1_apply(env16 closure_env_global_invoker_1, func_to_call__13 func(int32) int32, value__14 int32) int32 {
    var ret29 int32
    ret29 = apply_once(func_to_call__13, value__14)
    return ret29
}

func _goml_inherent_closure_env_composer_closure_2_closure_env_composer_closure_2_apply(env17 closure_env_composer_closure_2, value__17 int32) int32 {
    var ret30 int32
    ret30 = compose(double, increment, value__17)
    return ret30
}

func main() {
    main0()
}
