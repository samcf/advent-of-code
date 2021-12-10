package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var masks = []int64{0x1F00000, 0xF8000, 0x7C00, 0x3E0, 0x1F, 0x1084210, 0x842108, 0x421084, 0x210842, 0x108421}

func a(boards []map[int64]int, xs []int64) int64 {
	states := make([]int64, len(boards))
	called := map[int64]bool{}

	for _, x := range xs {
		called[x] = true
		for j, board := range boards {
			if idx, ok := board[x]; ok {
				states[j] = states[j] | (1 << (24 - idx))
				for _, mask := range masks {
					if (states[j] & mask) == mask {
						var t int64
						for k := range board {
							if _, ok := called[k]; !ok {
								t = t + k
							}
						}

						return t * x
					}
				}
			}
		}
	}

	return 0
}

func b(boards []map[int64]int, xs []int64) int64 {
	states := make([]int64, len(boards))
	victor := make([]bool, len(boards))

	a, b := 0, 0
	for i, x := range xs {
		for j, v := range boards {
			if idx, ok := v[x]; ok {
				states[j] |= 1 << (24 - idx)
				for _, mask := range masks {
					if victor[j] == false && ((states[j] & mask) == mask) {
						victor[j] = true
						a = i
						b = j
					}
				}
			}
		}
	}

	var t int64
	for _, x := range xs[a+1:] {
		if _, ok := boards[b][x]; ok {
			t += x
		}
	}

	return t * xs[a]
}

func parse() ([]map[int64]int, []int64) {
	scn := bufio.NewScanner(os.Stdin)
	scn.Scan()

	draws := []int64{}
	for _, s := range strings.Split(scn.Text(), ",") {
		x, _ := strconv.ParseInt(s, 10, 64)
		draws = append(draws, x)
	}

	a := [][]int64{}
	b := []int64{}
	for scn.Scan() {
		if scn.Text() == "" {
			if len(b) > 0 {
				a = append(a, b)
			}

			b = []int64{}
			continue
		}

		c := []int64{}
		for _, s := range strings.Fields(scn.Text()) {
			x, _ := strconv.ParseInt(s, 10, 64)
			c = append(c, x)
		}

		b = append(b, c...)
	}

	a = append(a, b)

	boards := []map[int64]int{}
	for _, xs := range a {
		board := map[int64]int{}
		for j, x := range xs {
			board[x] = j
		}

		boards = append(boards, board)
	}

	return boards, draws
}

func main() {
	boards, draws := parse()
	fmt.Printf("Part A: %d\n", a(boards, draws))
	fmt.Printf("Part B: %d\n", b(boards, draws))
}
