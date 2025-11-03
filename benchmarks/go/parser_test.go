package reflection

import (
	"encoding/json"
	"fmt"
	"testing"
)

var book string = "{\"name\":\"Jonh \",\"page_count\":124,\"rating\":1,\"release_year\":2016}"

func BenchmarkParsingBookUnmarshal(b *testing.B) {
	s := []byte(book)
	bookO := Book{}
	for b.Loop() {
		json.Unmarshal(s, &bookO)
	}
}

func BenchmarkParsingBookRefl(b *testing.B) {
	for b.Loop() {
		ParseBook(book)
	}
}

func mkBooks(size int) string {
	payload := "["
	for i := range size {
		if i < size-1 {
			payload = fmt.Sprintf("%s%s,", payload, book)
		} else {
			payload = fmt.Sprintf("%s%s", payload, book)
		}
	}

	payload = fmt.Sprintf("%s]", payload)
	return payload
}

func ParsingBooksRefl(b *testing.B, size int) {
	s := mkBooks(size)
	for b.Loop() {
		ParseBooks(s)
	}
}

func ParsingBooksUnmarshal(b *testing.B, size int) {
	books := []Book{}
	s := []byte(mkBooks(size))
	for b.Loop() {
		json.Unmarshal(s, &books)
	}
}

func BenchmarkParsingBooksUnmarshal_1(b *testing.B)  { ParsingBooksUnmarshal(b, 1) }
func BenchmarkParsingBooksRefl_1(b *testing.B)       { ParsingBooksRefl(b, 1) }
func BenchmarkParsingBooksUnmarshal_5(b *testing.B)  { ParsingBooksUnmarshal(b, 5) }
func BenchmarkParsingBooksRefl_5(b *testing.B)       { ParsingBooksRefl(b, 5) }
func BenchmarkParsingBooksUnmarshal_10(b *testing.B) { ParsingBooksUnmarshal(b, 10) }
func BenchmarkParsingBooksRefl_10(b *testing.B)      { ParsingBooksRefl(b, 10) }
func BenchmarkParsingBooksUnmarshal_15(b *testing.B) { ParsingBooksUnmarshal(b, 15) }
func BenchmarkParsingBooksRefl_15(b *testing.B)      { ParsingBooksRefl(b, 15) }
func BenchmarkParsingBooksUnmarshal_20(b *testing.B) { ParsingBooksUnmarshal(b, 20) }
func BenchmarkParsingBooksRefl_20(b *testing.B)      { ParsingBooksRefl(b, 20) }
