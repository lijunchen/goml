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
    var cell__0 *ref_int32_x
    var next__2 closure_env_next_0
    var reset__3 closure_env_reset_1
    var t15 Tuple2_closure_env_next_0_closure_env_reset_1
    cell__0 = ref__Ref_int32(0)
    next__2 = closure_env_next_0{
        cell_0: cell__0,
    }
    reset__3 = closure_env_reset_1{
        cell_0: cell__0,
    }
    t15 = Tuple2_closure_env_next_0_closure_env_reset_1{
        _0: next__2,
        _1: reset__3,
    }
    return t15
}

func main0() struct{} {
    var counter__4 Tuple2_closure_env_next_0_closure_env_reset_1
    var mtmp2 Tuple2_closure_env_next_0_closure_env_reset_1
    var x3 closure_env_next_0
    var x4 closure_env_reset_1
    var reset__6 closure_env_reset_1
    var next__5 closure_env_next_0
    var first__7 int32
    var second__8 int32
    var third__9 int32
    var new_counter__10 Tuple2_closure_env_next_0_closure_env_reset_1
    var mtmp6 Tuple2_closure_env_next_0_closure_env_reset_1
    var x7 closure_env_next_0
    var new_next__11 closure_env_next_0
    var fourth__12 int32
    var t16 string
    var t17 string
    var t18 string
    var t19 string
    counter__4 = make_counter()
    mtmp2 = counter__4
    x3 = mtmp2._0
    x4 = mtmp2._1
    reset__6 = x4
    next__5 = x3
    first__7 = _goml_inherent_closure_env_next_0_closure_env_next_0_apply(next__5)
    second__8 = _goml_inherent_closure_env_next_0_closure_env_next_0_apply(next__5)
    _goml_inherent_closure_env_reset_1_closure_env_reset_1_apply(reset__6)
    third__9 = _goml_inherent_closure_env_next_0_closure_env_next_0_apply(next__5)
    new_counter__10 = make_counter()
    mtmp6 = new_counter__10
    x7 = mtmp6._0
    new_next__11 = x7
    fourth__12 = _goml_inherent_closure_env_next_0_closure_env_next_0_apply(new_next__11)
    t16 = int32_to_string(first__7)
    string_println(t16)
    t17 = int32_to_string(second__8)
    string_println(t17)
    t18 = int32_to_string(third__9)
    string_println(t18)
    t19 = int32_to_string(fourth__12)
    string_println(t19)
    return struct{}{}
}

func _goml_inherent_closure_env_next_0_closure_env_next_0_apply(env13 closure_env_next_0) int32 {
    var cell__0 *ref_int32_x
    var t20 int32
    var next__1 int32
    cell__0 = env13.cell_0
    t20 = ref_get__Ref_int32(cell__0)
    next__1 = t20 + 1
    ref_set__Ref_int32(cell__0, next__1)
    return next__1
}

func _goml_inherent_closure_env_reset_1_closure_env_reset_1_apply(env14 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x
    cell__0 = env14.cell_0
    ref_set__Ref_int32(cell__0, 0)
    return struct{}{}
}

func main() {
    main0()
}
