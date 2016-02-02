## High-level coding ...

```rust
/// sums all positive values in `v`
fn sum_pos(v: &[i32]) -> i32 {
    let mut sum = 0;
    for i in v.iter().filter(|i| **i > 0) {
        sum += *i;
    }
    sum
}
```

<!-- NOTE: Above is *not* the actual source for playpen
     link. See a01_abstraction_source.md for that. -->

[sum_pos][]

## ... with low-level performance

```nasm
	xorl	%eax, %eax
	leaq	ref4197(%rip), %rcx
	leaq	ref4197+20(%rip), %rdx
	jmp	.LBB0_1
.LBB0_3:
	addl	%esi, %eax
	.align	16, 0x90
.LBB0_1:
	cmpq	%rcx, %rdx
	je	.LBB0_4
	movl	(%rcx), %esi
	addq	$4, %rcx
	testl	%esi, %esi
	jle	.LBB0_1
	jmp	.LBB0_3
.LBB0_4:
	retq
```
