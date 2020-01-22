#pragma once

//
// t2.h
//
// Copyright (C) 2012 - 2017 jones@scss.tcd.ie
//
// example of mixing C++ and x64 assembly language
//

//
// NB: "extern C" to avoid procedure name mangling by C++ compiler
//

extern "C" _int64 g;                                            // global int g
extern "C" _int64 min(_int64, _int64, _int64);                  // min
extern "C" _int64 p(_int64, _int64, _int64, _int64);            // p
extern "C" _int64 gcd(_int64, _int64);                          // gcd
extern "C" _int64 q(_int64, _int64, _int64, _int64, _int64);    // q
extern "C" _int64 qns();                                        // q no shadow

// eof