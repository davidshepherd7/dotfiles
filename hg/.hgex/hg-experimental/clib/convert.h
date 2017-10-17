/* convert.h - conversion utility methods
 *
 * Copyright 2016 Facebook, Inc.
 *
 * This software may be used and distributed according to the terms of the
 * GNU General Public License version 2 or any later version. */

#ifdef __cplusplus
#include <string>
#endif

#ifndef __CONVERT_H_C__
#define __CONVERT_H_C__

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

static const size_t BIN_NODE_SIZE = 20;
static const size_t HEX_NODE_SIZE = 40;

static const char *const NULLID = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
static const char *const HEXNULLID = "0000000000000000000000000000000000000000";

static const int8_t hextable[256] = {
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,  /*0-9*/
	-1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, /*A-F*/
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, /*a-f*/
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
};

static char chartable[16] = {
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
};

/*
 * Turn a hex-encoded string into binary.  Returns false on failure.
 */
static inline bool unhexlify(const char *input, int len, uint8_t *dst) {
	if (len % 2 != 0) {
		return false;
	}

	for (size_t ix = 0; ix < (size_t)len; ix += 2, dst++) {
		int hi = hextable[(unsigned char)input[ix]];
		int lo = hextable[(unsigned char)input[ix + 1]];

		if (hi < 0 || lo < 0) {
			return false;
		}
		*dst = (hi << 4) | lo;
	}

	return true;
}

/*
 * Turn binary data into a hex-encoded string.
 */
static inline void hexlify(const uint8_t *input, int len, char *dst) {
	for (size_t ix = 0; ix < (size_t)len; ix++, dst += 2) {
		unsigned char ch = (unsigned char)input[ix];
		char hi = chartable[ch >> 4];
		char lo = chartable[ch & 0xf];

		*dst = hi;
		*(dst + 1) = lo;
	}
}

#endif /* #ifndef __CONVERT_H_C__ */

#ifdef __cplusplus
#ifndef __CONVERT_H_CPP__
#define __CONVERT_H_CPP__

/**
 * Converts a given 40-byte hex string into a 20-byte node.
 */
static inline void appendbinfromhex(const char *node, std::string &output) {
	for (size_t i = 0; i < HEX_NODE_SIZE;) {
		int8_t hi = hextable[(unsigned char)node[i++]];
		int8_t lo = hextable[(unsigned char)node[i++]];
		output.push_back((hi << 4) | lo);
	}
}

/**
 * Converts a given 40-byte hex string into a 20-byte node.
 */
static inline std::string binfromhex(const char *node) {
	std::string result;

	result.reserve(BIN_NODE_SIZE);
	appendbinfromhex(node, result);
	return result;
}

/**
 * Converts a given 20-byte node into a 40-byte hex string.
 */
static inline void hexfrombin(const char *binnode, std::string &output) {
	for (size_t ix = 0; ix < BIN_NODE_SIZE; ix++) {
		unsigned char ch = (unsigned char)binnode[ix];
		char hi = chartable[ch >> 4];
		char lo = chartable[ch & 0xf];

		output.push_back(hi);
		output.push_back(lo);
	}
}

#endif /* #ifndef __CONVERT_H_CPP__ */
#endif
