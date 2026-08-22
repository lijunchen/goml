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

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func once_cell_new__OnceCell__isize() *OnceCell__isize {
    var cell *OnceCell__isize = &OnceCell__isize{}
    cell.cond = _goml_sync.NewCond(&cell.mutex)
    return cell
}

func once_cell_get_or_init__OnceCell__isize(cell *OnceCell__isize, init func() int) int {
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
        var initialized int = init()
        cell.mutex.Lock()
        cell.value = initialized
        cell.state = 2
        cell.owner = 0
        cell.cond.Broadcast()
        cell.mutex.Unlock()
        return initialized
    }
}

type Tuple2_3int_4bool struct {
    _0 int
    _1 bool
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {
    results_0 chan int
}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {
    results_0 chan int
}

type OnceCell__isize struct {
    mutex _goml_sync.Mutex
    cond *_goml_sync.Cond
    state int
    owner uint64
    value int
    name string
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

var VALUE *OnceCell__isize = func() *OnceCell__isize {
    var cell *OnceCell__isize = once_cell_new__OnceCell__isize()
    cell.name = "VALUE"
    return cell
}()

func main0() struct{} {
    var results__0 chan int
    var inline494 int = 2
    var inline495 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline494)
    results__0 = inline495
    var t418 closure_env_main_1 = closure_env_main_1{
        results_0: results__0,
    }
    var t419 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t418)
    }
    go t419()
    var t420 closure_env_main_3 = closure_env_main_3{
        results_0: results__0,
    }
    var t421 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t420)
    }
    go t421()
    var t422 Option__isize
    var inline487 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(results__0)
    var inline488 int = inline487._0
    var inline489 bool = inline487._1
    if inline489 {
        var inline492 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: inline488,
        }
        t422 = inline492
    } else {
        t422 = Option__isize{
            _tag: 0,
        }
    }
    var first__1 int
    var inline483 int = 0
    switch t422._tag {
    case 0:
        first__1 = inline483
    case 1:
        var inline484 int = t422._v1_0
        first__1 = inline484
    default:
        panic("non-exhaustive match")
    }
    var t423 Option__isize
    var inline476 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(results__0)
    var inline477 int = inline476._0
    var inline478 bool = inline476._1
    if inline478 {
        var inline481 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: inline477,
        }
        t423 = inline481
    } else {
        t423 = Option__isize{
            _tag: 0,
        }
    }
    var second__2 int
    var inline472 int = 0
    switch t423._tag {
    case 0:
        second__2 = inline472
    case 1:
        var inline473 int = t423._v1_0
        second__2 = inline473
    default:
        panic("non-exhaustive match")
    }
    var t424 bool = first__1 == second__2
    var t425 string
    var inline470 string = _goml_runtime_core_bool_to_string(t424)
    t425 = inline470
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline467)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env413 closure_env_main_0) int {
    return 41
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env414 closure_env_main_1) struct{} {
    var results__0 chan int = env414.results_0
    var t455 closure_env_main_0 = closure_env_main_0{}
    var t456 func() int = func() int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t455)
    }
    var t457 int
    var inline500 int = once_cell_get_or_init__OnceCell__isize(VALUE, t456)
    t457 = inline500
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(results__0, t457)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env415 closure_env_main_2) int {
    return 42
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env416 closure_env_main_3) struct{} {
    var results__0 chan int = env416.results_0
    var t462 closure_env_main_2 = closure_env_main_2{}
    var t463 func() int = func() int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t462)
    }
    var t464 int
    var inline504 int = once_cell_get_or_init__OnceCell__isize(VALUE, t463)
    t464 = inline504
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(results__0, t464)
    return struct{}{}
}

func main() {
    main0()
}
