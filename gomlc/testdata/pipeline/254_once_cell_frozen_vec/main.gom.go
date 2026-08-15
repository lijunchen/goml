package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
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

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func once_cell_new__OnceCell__FrozenVec__int() *OnceCell__FrozenVec__int {
    var cell *OnceCell__FrozenVec__int = &OnceCell__FrozenVec__int{}
    cell.cond = _goml_sync.NewCond(&cell.mutex)
    return cell
}

func once_cell_get_or_init__OnceCell__FrozenVec__int(cell *OnceCell__FrozenVec__int, init func() FrozenVec__int) FrozenVec__int {
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
        var initialized FrozenVec__int = init()
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
        items: _goml_slices.Grow([]int{}, int(capacity)),
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

type closure_env_values_0 struct {}

type FrozenVec__int struct {
    values *_goml_vec_int
}

type OnceCell__FrozenVec__int struct {
    mutex _goml_sync.Mutex
    cond *_goml_sync.Cond
    state int
    owner uint64
    value FrozenVec__int
    name string
}

type Ordering int32

var VALUES *OnceCell__FrozenVec__int = func() *OnceCell__FrozenVec__int {
    var cell *OnceCell__FrozenVec__int = once_cell_new__OnceCell__FrozenVec__int()
    cell.name = "VALUES"
    return cell
}()

func values() FrozenVec__int {
    var t414 closure_env_values_0 = closure_env_values_0{}
    var t415 func() FrozenVec__int = func() FrozenVec__int {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(t414)
    }
    var inline478 FrozenVec__int = once_cell_get_or_init__OnceCell__FrozenVec__int(VALUES, t415)
    return inline478
}

func main0() struct{} {
    var frozen__0 FrozenVec__int
    var inline517 closure_env_values_0 = closure_env_values_0{}
    var inline518 func() FrozenVec__int = func() FrozenVec__int {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline517)
    }
    var inline519 FrozenVec__int = _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(VALUES, inline518)
    frozen__0 = inline519
    var copy__1 *_goml_vec_int
    var inline514 *_goml_vec_int = frozen__0.values
    var inline515 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(inline514)
    copy__1 = inline515
    var inline510 int = 0
    var inline511 int = 9
    vec_set__Vec_3int(copy__1, inline510, inline511)
    var t418 int
    var inline506 int = 0
    var inline507 *_goml_vec_int = frozen__0.values
    var inline508 int = vec_get__Vec_3int(inline507, inline506)
    t418 = inline508
    var t419 string
    var inline504 string = _goml_runtime_core_int_to_string(t418)
    t419 = inline504
    var inline501 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline501)
    var t420 int
    var inline498 int = 0
    var inline499 int = vec_get__Vec_3int(copy__1, inline498)
    t420 = inline499
    var t421 string
    var inline496 string = _goml_runtime_core_int_to_string(t420)
    t421 = inline496
    var inline493 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline493)
    var t422 FrozenVec__int
    var inline489 closure_env_values_0 = closure_env_values_0{}
    var inline490 func() FrozenVec__int = func() FrozenVec__int {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline489)
    }
    var inline491 FrozenVec__int = _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(VALUES, inline490)
    t422 = inline491
    var t423 int
    var inline485 int = 1
    var inline486 *_goml_vec_int = t422.values
    var inline487 int = vec_get__Vec_3int(inline486, inline485)
    t423 = inline487
    var t424 string
    var inline483 string = _goml_runtime_core_int_to_string(t423)
    t424 = inline483
    var inline480 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline480)
    return struct{}{}
}

func _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(self__402 *OnceCell__FrozenVec__int, init__403 func() FrozenVec__int) FrozenVec__int {
    var t432 FrozenVec__int = once_cell_get_or_init__OnceCell__FrozenVec__int(self__402, init__403)
    return t432
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(self__264 *_goml_vec_int) *_goml_vec_int {
    var t454 int
    var inline528 int = vec_len__Vec_3int(self__264)
    t454 = inline528
    var result__265 *_goml_vec_int
    var inline526 *_goml_vec_int = vec_with_capacity__Vec_3int(t454)
    result__265 = inline526
    var index__266 int = 0
    Loop_loop456:
    for {
        var t457 int
        var inline524 int = vec_len__Vec_3int(self__264)
        t457 = inline524
        var t458 bool = index__266 < t457
        if t458 {
            var t459 int = vec_get__Vec_3int(self__264, index__266)
            vec_push__Vec_3int(result__265, t459)
            var compound_old196 int = index__266
            var compound_value197 int = 1
            var t460 int = compound_old196 + compound_value197
            index__266 = t460
            continue
        } else {
            break Loop_loop456
        }
    }
    return result__265
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(env411 closure_env_values_0) FrozenVec__int {
    var t474 [3]int = [3]int{1, 2, 3}
    var t475 *_goml_vec_int = func(values [3]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t474)
    var inline530 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(t475)
    var inline531 FrozenVec__int = FrozenVec__int{
        values: inline530,
    }
    return inline531
}

func main() {
    main0()
}
