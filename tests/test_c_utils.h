#pragma once

// Helper header file for testing fdp_c_utils.f90.
// Exposes interface for C functions that work with strings

// Return 1 if str is "foo". Otherwise return "bar".
int is_foo(const char* str);

// Repeat string, doubling its length
// User is responsible for setting up output buffer!
void repeat_str(const char* input, char* output);
