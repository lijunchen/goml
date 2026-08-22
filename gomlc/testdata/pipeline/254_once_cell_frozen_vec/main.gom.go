package main

import (
    _goml_os "os"
    _goml_sync "sync"
    _goml_runtime_pkg "runtime"
)

func _goml_once_cell_goroutine_id() uint64 {
    var buffer []uint8 = make([]uint8, 64)
    var length int = _goml_runtime_pkg.Stack(buffer, false)
    var index int = 10
    var result uint64 = 0
    for {
        if index >= length {
            break
        }
        if buffer[index] < 48 || buffer[index] > 57 {
            break
        }
        result = result * 10 + uint64(buffer[index] - 48)
        index = index + 1
    }
    return result
}

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

func once_cell_new__OnceCell__FrozenVec__isize() *OnceCell__FrozenVec__isize {
    var cell *OnceCell__FrozenVec__isize = &OnceCell__FrozenVec__isize{}
    cell.cond = _goml_sync.NewCond(&cell.mutex)
    return cell
}

func once_cell_get_or_init__OnceCell__FrozenVec__isize(cell *OnceCell__FrozenVec__isize, init func() FrozenVec__isize) FrozenVec__isize {
    cell.mutex.Lock()
    for {
        if cell.state == 2 {
            cell.mutex.Unlock()
            return cell.value
        }
        var goroutine uint64 = _goml_once_cell_goroutine_id()
        if cell.state == 1 {
            if cell.owner == goroutine {
                cell.mutex.Unlock()
                panic("recursive OnceCell initialization: " + cell.name)
            }
            cell.cond.Wait()
            continue
        }
        cell.state = 1
        cell.owner = goroutine
        cell.mutex.Unlock()
        var initialized FrozenVec__isize = init()
        cell.mutex.Lock()
        cell.value = initialized
        cell.state = 2
        cell.owner = 0
        cell.cond.Broadcast()
        cell.mutex.Unlock()
        return initialized
    }
}

type _goml_vec_int struct {
    items []int
}

func vec_with_capacity__Vec_3int(capacity int) *_goml_vec_int {
    return &_goml_vec_int{
        items: make([]int, 0, capacity),
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_set__Vec_3int(vec *_goml_vec_int, index int, value int) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
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

type closure_env_values_0 struct {}

type FrozenVec__isize struct {
    values *_goml_vec_int
}

type OnceCell__FrozenVec__isize struct {
    mutex _goml_sync.Mutex
    cond *_goml_sync.Cond
    state int
    owner uint64
    value FrozenVec__isize
    name string
}

type Ordering int32

var VALUES *OnceCell__FrozenVec__isize = func() *OnceCell__FrozenVec__isize {
    var cell *OnceCell__FrozenVec__isize = once_cell_new__OnceCell__FrozenVec__isize()
    cell.name = "VALUES"
    return cell
}()

func values() FrozenVec__isize {
    var t802 closure_env_values_0 = closure_env_values_0{}
    var t803 func() FrozenVec__isize = func() FrozenVec__isize {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(t802)
    }
    var inline909 FrozenVec__isize = once_cell_get_or_init__OnceCell__FrozenVec__isize(VALUES, t803)
    return inline909
}

func main0() struct{} {
    var frozen__0 FrozenVec__isize
    var inline948 closure_env_values_0 = closure_env_values_0{}
    var inline949 func() FrozenVec__isize = func() FrozenVec__isize {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline948)
    }
    var inline950 FrozenVec__isize = _goml_m_inherent_i_OnceCell_i__hc919cf300d97b8e399cb9d4664fdfe6e_nVec_l_isize_r_(VALUES, inline949)
    frozen__0 = inline950
    var copy__1 *_goml_vec_int
    var inline945 *_goml_vec_int = frozen__0.values
    var inline946 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__isize(inline945)
    copy__1 = inline946
    var inline941 int = 0
    var inline942 int = 9
    vec_set__Vec_3int(copy__1, inline941, inline942)
    var t806 int
    var inline937 int = 0
    var inline938 *_goml_vec_int = frozen__0.values
    var inline939 int = vec_get__Vec_3int(inline938, inline937)
    t806 = inline939
    var t807 string
    var inline935 string = __goml_builtin_int_to_string(t806)
    t807 = inline935
    var inline932 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t807)
    _goml_runtime_core_string_println(inline932)
    var t808 int
    var inline929 int = 0
    var inline930 int = vec_get__Vec_3int(copy__1, inline929)
    t808 = inline930
    var t809 string
    var inline927 string = __goml_builtin_int_to_string(t808)
    t809 = inline927
    var inline924 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t809)
    _goml_runtime_core_string_println(inline924)
    var t810 FrozenVec__isize
    var inline920 closure_env_values_0 = closure_env_values_0{}
    var inline921 func() FrozenVec__isize = func() FrozenVec__isize {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline920)
    }
    var inline922 FrozenVec__isize = _goml_m_inherent_i_OnceCell_i__hc919cf300d97b8e399cb9d4664fdfe6e_nVec_l_isize_r_(VALUES, inline921)
    t810 = inline922
    var t811 int
    var inline916 int = 1
    var inline917 *_goml_vec_int = t810.values
    var inline918 int = vec_get__Vec_3int(inline917, inline916)
    t811 = inline918
    var t812 string
    var inline914 string = __goml_builtin_int_to_string(t811)
    t812 = inline914
    var inline911 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
    _goml_runtime_core_string_println(inline911)
    return struct{}{}
}

func _goml_m_inherent_i_OnceCell_i__hc919cf300d97b8e399cb9d4664fdfe6e_nVec_l_isize_r_(self__655 *OnceCell__FrozenVec__isize, init__656 func() FrozenVec__isize) FrozenVec__isize {
    var t820 FrozenVec__isize = once_cell_get_or_init__OnceCell__FrozenVec__isize(self__655, init__656)
    return t820
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__isize(self__517 *_goml_vec_int) *_goml_vec_int {
    var t842 int
    var inline962 int = vec_len__Vec_3int(self__517)
    t842 = inline962
    var result__518 *_goml_vec_int
    var inline960 *_goml_vec_int = vec_with_capacity__Vec_3int(t842)
    result__518 = inline960
    var index__519 int = 0
    Loop_loop844:
    for {
        var t845 int
        var inline958 int = vec_len__Vec_3int(self__517)
        t845 = inline958
        var t846 bool = index__519 < t845
        if t846 {
            var t847 int = vec_get__Vec_3int(self__517, index__519)
            vec_push__Vec_3int(result__518, t847)
            var compound_old581 int = index__519
            var compound_value582 int = 1
            var t848 int = compound_old581 + compound_value582
            index__519 = t848
            continue
        } else {
            break Loop_loop844
        }
    }
    return result__518
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t854 int64 = int64(int(value__222))
    var inline964 bool = t854 < 0
    if inline964 {
        var inline965 uint64 = uint64(int64(t854))
        var inline966 uint64 = 0 - inline965
        var inline967 string = decimal_string(inline966)
        var inline968 string = "-" + inline967
        return inline968
    } else {
        var inline969 uint64 = uint64(int64(t854))
        var inline970 string = decimal_string(inline969)
        return inline970
    }
}

func decimal_string(value__208 uint64) string {
    var t897 bool = value__208 == 0
    if t897 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop890:
        for {
            var t891 bool = remaining__210 > 0
            if t891 {
                var t892_rhs uint64 = 10
                var t892 uint64 = remaining__210 % t892_rhs
                var t893 uint8 = uint8(uint64(t892))
                var t894 uint8 = t893 + 48
                vec_push__Vec_5uint8(reversed__209, t894)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t895 uint64 = compound_old353 / compound_value354
                remaining__210 = t895
                continue
            } else {
                break Loop_loop890
            }
        }
        var t879 int
        var inline980 int = vec_len__Vec_5uint8(reversed__209)
        t879 = inline980
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t879)
        var offset__212 int = 0
        Loop_loop881:
        for {
            var t882 int
            var inline978 int = vec_len__Vec_5uint8(reversed__209)
            t882 = inline978
            var t883 bool = offset__212 < t882
            if t883 {
                var t884 int
                var inline976 int = vec_len__Vec_5uint8(reversed__209)
                t884 = inline976
                var t885 int = t884 - offset__212
                var t886 int = t885 - 1
                var t887 uint8 = vec_get__Vec_5uint8(reversed__209, t886)
                vec_push__Vec_5uint8(bytes__211, t887)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t888 int = compound_old358 + compound_value359
                offset__212 = t888
                continue
            } else {
                break Loop_loop881
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(env799 closure_env_values_0) FrozenVec__isize {
    var t905 [3]int = [3]int{1, 2, 3}
    var t906 *_goml_vec_int = func(values [3]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [3]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t905)
    var inline982 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__isize(t906)
    var inline983 FrozenVec__isize = FrozenVec__isize{
        values: inline982,
    }
    return inline983
}

func main() {
    main0()
}
