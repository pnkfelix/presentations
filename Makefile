test-c++: c++-cake
	./$<

c++-cake: cake.cpp
	clang++ -Wall -o $@ $<
