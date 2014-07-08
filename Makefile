RUSTC ?= rustc

default: build-all-dictionary test-dictionary-v1 run-bd-96-concrete-wrapper

test-%: %.test
	./$< --test

run-%: %.bin
	./$<

%.bin: %.rs
	$(RUSTC) $< -o $@

%.test: %.rs
	$(RUSTC) --cfg test $< -o $@

build-all-dictionary: dictionary-v1.rs
	$(RUSTC) --test --cfg version1 $< --out-dir /tmp
	$(RUSTC) --test --cfg version2 $< --out-dir /tmp
	$(RUSTC) --test --cfg version3 $< --out-dir /tmp
	$(RUSTC) --test --cfg version4 $< --out-dir /tmp
