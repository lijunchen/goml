package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type closure_env_f4_0 struct {
    a_0 int32
    b_1 int32
    c_2 int32
    d_3 int32
    x_4 int32
    y_5 int32
    z_6 int32
}

type closure_env_f3_1 struct {
    a_0 int32
    b_1 int32
    c_2 int32
    x_3 int32
    y_4 int32
}

type closure_env_f2_2 struct {
    a_0 int32
    b_1 int32
    x_2 int32
}

type closure_env_f1_3 struct {
    a_0 int32
}

type Ordering int32

func main0() struct{} {
    var a__0 int32 = 10
    var t801 closure_env_f1_3 = closure_env_f1_3{
        a_0: a__0,
    }
    var f1__11 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f1__3_i_closure__env__f1__3_i_apply(t801, p0)
    }
    var result__12 int32 = f1__11(1)
    var inline874 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(result__12)
    _goml_runtime_core_string_println(inline874)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline879 int64 = int64(int32(self__407))
    var inline880 string = signed_decimal_string(inline879)
    return inline880
}

func signed_decimal_string(value__214 int64) string {
    var t817 bool = value__214 < 0
    if t817 {
        var t818 uint64 = uint64(int64(value__214))
        var t819 uint64 = 0 - t818
        var t820 string = decimal_string(t819)
        var t821 string = "-" + t820
        return t821
    } else {
        var t822 uint64 = uint64(int64(value__214))
        var t823 string = decimal_string(t822)
        return t823
    }
}

func decimal_string(value__208 uint64) string {
    var t846 bool = value__208 == 0
    if t846 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop839:
        for {
            var t840 bool = remaining__210 > 0
            if t840 {
                var t841_rhs uint64 = 10
                var t841 uint64 = remaining__210 % t841_rhs
                var t842 uint8 = uint8(uint64(t841))
                var t843 uint8 = t842 + 48
                vec_push__Vec_5uint8(reversed__209, t843)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t844 uint64 = compound_old353 / compound_value354
                remaining__210 = t844
                continue
            } else {
                break Loop_loop839
            }
        }
        var t828 int
        var inline898 int = vec_len__Vec_5uint8(reversed__209)
        t828 = inline898
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t828)
        var offset__212 int = 0
        Loop_loop830:
        for {
            var t831 int
            var inline896 int = vec_len__Vec_5uint8(reversed__209)
            t831 = inline896
            var t832 bool = offset__212 < t831
            if t832 {
                var t833 int
                var inline894 int = vec_len__Vec_5uint8(reversed__209)
                t833 = inline894
                var t834 int = t833 - offset__212
                var t835 int = t834 - 1
                var t836 uint8 = vec_get__Vec_5uint8(reversed__209, t835)
                vec_push__Vec_5uint8(bytes__211, t836)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t837 int = compound_old358 + compound_value359
                offset__212 = t837
                continue
            } else {
                break Loop_loop830
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(env796 closure_env_f4_0, w__7 int32) int32 {
    var a__0 int32 = env796.a_0
    var b__2 int32 = env796.b_1
    var c__4 int32 = env796.c_2
    var d__6 int32 = env796.d_3
    var x__1 int32 = env796.x_4
    var y__3 int32 = env796.y_5
    var z__5 int32 = env796.z_6
    var t854 int32 = a__0 + b__2
    var t855 int32 = t854 + c__4
    var t856 int32 = t855 + d__6
    var t857 int32 = t856 + x__1
    var t858 int32 = t857 + y__3
    var t859 int32 = t858 + z__5
    var t860 int32 = t859 + w__7
    return t860
}

func _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(env797 closure_env_f3_1, z__5 int32) int32 {
    var a__0 int32 = env797.a_0
    var b__2 int32 = env797.b_1
    var c__4 int32 = env797.c_2
    var x__1 int32 = env797.x_3
    var y__3 int32 = env797.y_4
    var d__6 int32 = 40
    var t863 closure_env_f4_0 = closure_env_f4_0{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        d_3: d__6,
        x_4: x__1,
        y_5: y__3,
        z_6: z__5,
    }
    var f4__8 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(t863, p0)
    }
    var t864 int32 = f4__8(4)
    return t864
}

func _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(env798 closure_env_f2_2, y__3 int32) int32 {
    var a__0 int32 = env798.a_0
    var b__2 int32 = env798.b_1
    var x__1 int32 = env798.x_2
    var c__4 int32 = 30
    var t867 closure_env_f3_1 = closure_env_f3_1{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        x_3: x__1,
        y_4: y__3,
    }
    var f3__9 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(t867, p0)
    }
    var t868 int32 = f3__9(3)
    return t868
}

func _goml_m_inherent_i_closure__env__f1__3_i_closure__env__f1__3_i_apply(env799 closure_env_f1_3, x__1 int32) int32 {
    var a__0 int32 = env799.a_0
    var b__2 int32 = 20
    var t871 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: b__2,
        x_2: x__1,
    }
    var f2__10 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(t871, p0)
    }
    var t872 int32 = f2__10(2)
    return t872
}

func main() {
    main0()
}
