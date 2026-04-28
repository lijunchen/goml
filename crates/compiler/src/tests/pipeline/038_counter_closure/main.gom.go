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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit struct {
    _0 func() int32
    _1 func() struct{}
}

type closure_env_next_0 struct {
    cell_0 *ref_int32_x
}

type closure_env_reset_1 struct {
    cell_0 *ref_int32_x
}

type GoError = error

func make_counter() Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit {
    var retv16 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit
    var cell__0 *ref_int32_x = ref__Ref_5int32(0)
    var next__2 closure_env_next_0 = closure_env_next_0{
        cell_0: cell__0,
    }
    var reset__3 closure_env_reset_1 = closure_env_reset_1{
        cell_0: cell__0,
    }
    var t17 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit{
        _0: func() int32 {
            return _goml_inherent_x23_closure_x5f_env_x5f_next_x5f_0_x23_closure_x5f_env_x5f_next_x5f_0_x23_apply(next__2)
        },
        _1: func() struct{} {
            return _goml_inherent_x23_closure_x5f_env_x5f_reset_x5f_1_x23_closure_x5f_env_x5f_reset_x5f_1_x23_apply(reset__3)
        },
    }
    retv16 = t17
    return retv16
}

func main0() struct{} {
    var counter__4 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = make_counter()
    var mtmp2 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = counter__4
    var x3 func() int32 = mtmp2._0
    var x4 func() struct{} = mtmp2._1
    var reset__6 func() struct{} = x4
    var next__5 func() int32 = x3
    var first__7 int32 = next__5()
    var second__8 int32 = next__5()
    reset__6()
    var third__9 int32 = next__5()
    var new_counter__10 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = make_counter()
    var mtmp6 Tuple2_17TFunc0_ret_5int32_16TFunc0_ret_4unit = new_counter__10
    var x7 func() int32 = mtmp6._0
    var new_next__11 func() int32 = x7
    var fourth__12 int32 = new_next__11()
    var t19 string = int32_to_string(first__7)
    string_println(t19)
    var t20 string = int32_to_string(second__8)
    string_println(t20)
    var t21 string = int32_to_string(third__9)
    string_println(t21)
    var t22 string = int32_to_string(fourth__12)
    string_println(t22)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_next_x5f_0_x23_closure_x5f_env_x5f_next_x5f_0_x23_apply(env13 closure_env_next_0) int32 {
    var retv24 int32
    var cell__0 *ref_int32_x = env13.cell_0
    var t25 int32 = ref_get__Ref_5int32(cell__0)
    var next__1 int32 = t25 + 1
    ref_set__Ref_5int32(cell__0, next__1)
    retv24 = next__1
    return retv24
}

func _goml_inherent_x23_closure_x5f_env_x5f_reset_x5f_1_x23_closure_x5f_env_x5f_reset_x5f_1_x23_apply(env14 closure_env_reset_1) struct{} {
    var cell__0 *ref_int32_x = env14.cell_0
    ref_set__Ref_5int32(cell__0, 0)
    return struct{}{}
}

func main() {
    main0()
}
