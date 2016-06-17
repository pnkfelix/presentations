# Soundness and 3rd Party Concurrency

## The Secret Sauce

 * `Send`

 * `Sync`

 * lifetime bounds

## Send and Sync

`T: Send` means an instance of `T` can be *transferred* between threads

(i.e. move or copied as appropriate)

. . .

`T: Sync` means two threads can safely *share* a reference to an instance of `T`

## Examples

`T: Send` : `T` can be *transferred* between threads

`T: Sync` : two threads can *share* refs to a `T`

 >- `String` is `Send`
 >- `Vec<T>` is `Send` (if `T` is `Send`)
 >- (double-check: why not require `T: Sync` for `Vec<T>: Send`?)
 >- `Rc<T>` is *not* `Send` (for any `T`)
 >- but `Arc<T>` *is* `Send` (if `T` is `Send` and `Sync`)
 >- (to ponder: why require `T:Send` for `Arc<T>`?)
 >- `&T` is `Send` if `T: Sync`
 >- `&mut T` is `Send` if `T: Send`

## Send and Sync are only half the story {.big_text .center}

### other half is lifetime bounds; come see me if curious
