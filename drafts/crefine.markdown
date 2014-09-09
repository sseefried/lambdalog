# Notes on the C Kernel source code and C refine proof

## C `include` files

### `types.h`

* `bool_t` is just an enum with `false = 0` and `true = 1`
* Three region types:
    - `region_t`
    - `p_region_t`
    - `v_region_t`.

    They are all `struct`s with fields `start` and `end`, however they
    have types (respectively)
    - `pptr_t`
    - `paddr_t`
    - `vptr_t`

#### Definitions 

* `pde_range` Page Directory? 
* `pte_range` Page Table? 
* `cte_range` Capability Tree?

What does the "e" stand for?

