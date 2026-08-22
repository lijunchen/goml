package main

import (
    _goml_fmt "fmt"
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
    var t417 closure_env_values_0 = closure_env_values_0{}
    var t418 func() FrozenVec__isize = func() FrozenVec__isize {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(t417)
    }
    var inline481 FrozenVec__isize = once_cell_get_or_init__OnceCell__FrozenVec__isize(VALUES, t418)
    return inline481
}

func main0() struct{} {
    var frozen__0 FrozenVec__isize
    var inline520 closure_env_values_0 = closure_env_values_0{}
    var inline521 func() FrozenVec__isize = func() FrozenVec__isize {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline520)
    }
    var inline522 FrozenVec__isize = _goml_m_inherent_i_OnceCell_i__hc919cf300d97b8e399cb9d4664fdfe6e_nVec_l_isize_r_(VALUES, inline521)
    frozen__0 = inline522
    var copy__1 *_goml_vec_int
    var inline517 *_goml_vec_int = frozen__0.values
    var inline518 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__isize(inline517)
    copy__1 = inline518
    var inline513 int = 0
    var inline514 int = 9
    vec_set__Vec_3int(copy__1, inline513, inline514)
    var t421 int
    var inline509 int = 0
    var inline510 *_goml_vec_int = frozen__0.values
    var inline511 int = vec_get__Vec_3int(inline510, inline509)
    t421 = inline511
    var t422 string
    var inline507 string = _goml_runtime_core_int_to_string(t421)
    t422 = inline507
    var inline504 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline504)
    var t423 int
    var inline501 int = 0
    var inline502 int = vec_get__Vec_3int(copy__1, inline501)
    t423 = inline502
    var t424 string
    var inline499 string = _goml_runtime_core_int_to_string(t423)
    t424 = inline499
    var inline496 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline496)
    var t425 FrozenVec__isize
    var inline492 closure_env_values_0 = closure_env_values_0{}
    var inline493 func() FrozenVec__isize = func() FrozenVec__isize {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline492)
    }
    var inline494 FrozenVec__isize = _goml_m_inherent_i_OnceCell_i__hc919cf300d97b8e399cb9d4664fdfe6e_nVec_l_isize_r_(VALUES, inline493)
    t425 = inline494
    var t426 int
    var inline488 int = 1
    var inline489 *_goml_vec_int = t425.values
    var inline490 int = vec_get__Vec_3int(inline489, inline488)
    t426 = inline490
    var t427 string
    var inline486 string = _goml_runtime_core_int_to_string(t426)
    t427 = inline486
    var inline483 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline483)
    return struct{}{}
}

func _goml_m_inherent_i_OnceCell_i__hc919cf300d97b8e399cb9d4664fdfe6e_nVec_l_isize_r_(self__402 *OnceCell__FrozenVec__isize, init__403 func() FrozenVec__isize) FrozenVec__isize {
    var t435 FrozenVec__isize = once_cell_get_or_init__OnceCell__FrozenVec__isize(self__402, init__403)
    return t435
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__isize(self__264 *_goml_vec_int) *_goml_vec_int {
    var t457 int
    var inline531 int = vec_len__Vec_3int(self__264)
    t457 = inline531
    var result__265 *_goml_vec_int
    var inline529 *_goml_vec_int = vec_with_capacity__Vec_3int(t457)
    result__265 = inline529
    var index__266 int = 0
    Loop_loop459:
    for {
        var t460 int
        var inline527 int = vec_len__Vec_3int(self__264)
        t460 = inline527
        var t461 bool = index__266 < t460
        if t461 {
            var t462 int = vec_get__Vec_3int(self__264, index__266)
            vec_push__Vec_3int(result__265, t462)
            var compound_old196 int = index__266
            var compound_value197 int = 1
            var t463 int = compound_old196 + compound_value197
            index__266 = t463
            continue
        } else {
            break Loop_loop459
        }
    }
    return result__265
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(env414 closure_env_values_0) FrozenVec__isize {
    var t477 [3]int = [3]int{1, 2, 3}
    var t478 *_goml_vec_int = func(values [3]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [3]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t477)
    var inline533 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__isize(t478)
    var inline534 FrozenVec__isize = FrozenVec__isize{
        values: inline533,
    }
    return inline534
}

func main() {
    main0()
}
