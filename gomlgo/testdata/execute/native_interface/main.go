package main

import "io"

type reader struct {
	data []byte
	offset int
}

func (value *reader) Read(buffer []byte) (int, error) {
	count := copy(buffer, value.data[value.offset:])
	value.offset += count
	return count, nil
}

func main() {
	value := &reader{data: []byte{'g', 'o', 'm', 'l'}}
	limited := io.LimitReader(value, 4)
	result, err := io.ReadAll(limited)
	println(len(result), result[0], result[1], result[2], result[3], err == nil, value.offset)
}
