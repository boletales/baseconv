# baseconv

## Description

A simple application for converting numbers between base10, base16BE, base16LE.

## Usage

Bin file `base` automatically detects the base.

- if the input contains only digits (0-9), it is treated as base10
- if the input starts with `0x`, it is treated as base16BE
- if the input contains only hex digits (0-9, a-f, A-F), it is treated as base16BE
- if the input contains hex digits and **spaces** (like `CA FE` as xxd dumps), it is treated as base16LE

example:
```
$ base 1000
Parsed input '1000' as dec
  dec: 1000
  hexBE: 0x03E8
  hexLE: E8 03

$ base 0x1000
Parsed input '0x1000' as hexBE
  dec: 4096
  hexBE: 0x1000
  hexLE: 00 10

$ base cafe
Parsed input 'cafe' as hexBE
  dec: 51966
  hexBE: 0xCAFE
  hexLE: FE CA

$ base CA FE
Parsed input 'CA FE' as hexLE
  dec: 65226
  hexBE: 0xFECA
  hexLE: CA FE
```

You can also specify the base explicitly with `-d` for decimal, `-b` for big-endian hex, and `-l` for little-endian hex.

```
$ base 1234 -d
Parsed input '1234' as dec
  dec: 1234
  hexBE: 0x4D2
  hexLE: D2 04

$ base 1234 -b
Parsed input '1234' as hexBE
  dec: 4660
  hexBE: 0x1234
  hexLE: 34 12

$ base 1234 -l
Parsed input '1234' as hexLE
  dec: 13330
  hexBE: 0x3412
  hexLE: 12 34
```