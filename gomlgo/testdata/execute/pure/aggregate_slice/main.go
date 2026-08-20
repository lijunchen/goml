package main

type pair struct {
	left  int
	right int
}

func main() {
	array := [3]int{1, 2, 3}
	copied := array
	copied[0] = 9
	value := pair{4, 5}
	pointer := new(value)
	(*pointer).left = 7
	slice := make([]int, 2, 4)
	slice[0] = array[0]
	slice[1] = value.right
	slice = append(slice, 6, 7, 8)
	target := []int{0, 0, 0}
	count := copy(target, slice[1:4])
	view := array[1:3]
	view[0] = 12
	arrayPointer := &array
	pointerView := arrayPointer[2:3]
	pointerView[0] = 13
	keyedArray := [4]int{3: 8, 0: 2}
	keyedSlice := []int{2: 9}
	keyedPair := pair{right: 11, left: 10}
	records := []pair{{left: 1, right: 2}}
	records[0].left = 21
	pointer.right = 22
	assignment := []int{10, 20}
	position := 0
	position, assignment[position] = 1, 99
	println(array[0], copied[0], value.left, pointer.left, len(slice), count, target[0], target[2], array[1], array[2], keyedArray[0], keyedArray[3], len(keyedSlice), keyedSlice[2], keyedPair.left, keyedPair.right, records[0].left, pointer.right, position, assignment[0], assignment[1])
}
