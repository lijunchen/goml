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

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_closure_env_next_0_closure_env_reset_1 struct {
    _0 closure_env_next_0
    _1 closure_env_reset_1
}

type closure_env_next_0 struct {
    cell_0 *ref_int32_x
}

type closure_env_reset_1 struct {
    cell_0 *ref_int32_x
}

func make_counter() Tuple2_closure_env_next_0_closure_env_reset_1 {
    var ret20 Tuple2_closure_env_next_0_closure_env_reset_1
    var cell__0 *ref_int32_x = ref__Ref_int32(0)
    var next__2 closure_env_next_0 = closure_env_next_0{
        cell_0: cell__0,
    }
    var reset__3 closure_env_reset_1 = closure_env_reset_1{
        cell_0: cell__0,
    }
    ret20 = Tuple2_closure_env_next_0_closure_env_reset_1{
        _0: next__2,
        _1: reset__3,
    }
    return ret20
}

func main0() struct{} {
    var ret21 struct{}
    var counter__4 Tuple2_closure_env_next_0_closure_env_reset_1 = make_counter()
    var mtmp2 Tuple2_closure_env_next_0_closure_env_reset_1 = counter__4
    var x3 closure_env_next_0 = mtmp2._0
    var x4 closure_env_reset_1 = mtmp2._1
    var reset__6 closure_env_reset_1 = x4
    var next__5 closure_env_next_0 = x3
    var first__7 int32 = goml__inherent_x23_closure__env__next__0_x23_closure__env__next__0_x23_apply(next__5)
    var second__8 int32 = goml__inherent_x23_closure__env__next__0_x23_closure__env__next__0_x23_apply(next__5)
    goml__inherent_x23_closure__env__reset__1_x23_closure__env__reset__1_x23_apply(reset__6)
    var third__9 int32 = goml__inherent_x23_closure__env__next__0_x23_closure__env__next__0_x23_apply(next__5)
    var new_counter__10 Tuple2_closure_env_next_0_closure_env_reset_1 = make_counter()
    var mtmp6 Tuple2_closure_env_next_0_closure_env_reset_1 = new_counter__10
    var x7 closure_env_next_0 = mtmp6._0
    var new_next__11 closure_env_next_0 = x7
    var fourth__12 int32 = goml__inherent_x23_closure__env__next__0_x23_closure__env__next__0_x23_apply(new_next__11)
    var t15 string = int32_to_string(first__7)
    string_println(t15)
    var t16 string = int32_to_string(second__8)
    string_println(t16)
    var t17 string = int32_to_string(third__9)
    string_println(t17)
    var t18 string = int32_to_string(fourth__12)
    string_println(t18)
    ret21 = struct{}{}
    return ret21
}

func goml__inherent_x23_closure__env__next__0_x23_closure__env__next__0_x23_apply(env13 closure_env_next_0) int32 {
    var ret22 int32
    var cell__0 *ref_int32_x = env13.cell_0
    var t19 int32 = ref_get__Ref_int32(cell__0)
    var next__1 int32 = t19 + 1
    ref_set__Ref_int32(cell__0, next__1)
    ret22 = next__1
    return ret22
}

func goml__inherent_x23_closure__env__reset__1_x23_closure__env__reset__1_x23_apply(env14 closure_env_reset_1) struct{} {
    var ret23 struct{}
    var cell__0 *ref_int32_x = env14.cell_0
    ref_set__Ref_int32(cell__0, 0)
    ret23 = struct{}{}
    return ret23
}

func main() {
    main0()
}
