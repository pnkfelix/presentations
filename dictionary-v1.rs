use std::hash::Hash;
use std::hash::sip;
static NUM_BUCKETS: uint = 17;

// (Rust has tuples `(A,B)`; using explicit struct to ease presentation.)
pub struct Entry<A,B> { key: A, data: B }

pub type T<A,B> = Table<A,B>;
pub struct Table<A,B> {
    length: uint,
    buckets: Vec<Vec<Entry<A,B>>>
}

// (during presentation, first present with non `&A` and explain why
// that is wrong in Rust here.)
fn hash_bucket<A:Hash>(key: &A) -> uint { sip::hash(key) as uint % NUM_BUCKETS }

pub fn create<A,B>() -> Table<A,B> {
    Table { length: 0,
            buckets: Vec::from_fn(NUM_BUCKETS, |_ignore| Vec::new()) }
}

pub fn length<A,B>(t: &Table<A,B>) -> uint { t.length }

#[cfg(not(version2), not(version3), not(version4))]
pub fn find<A:Eq+Hash,B:Clone>(t: &Table<A,B>, key: &A) -> Option<B> {
    let bucket : &Vec<Entry<A,B>> = t.buckets.get(hash_bucket(key));

    for i in range(0u, bucket.len()) {
        let entry = bucket.get(i);
        if entry.key == *key {
            return Some(entry.data.clone())
        }
    }
    None
}

#[cfg(version2)]
pub fn find<A:Eq+Hash,B:Clone>(t: &Table<A,B>, key: &A) -> Option<B> {
    let bucket : &Vec<Entry<A,B>> = t.buckets.get(hash_bucket(key));

    for i in range(0u, bucket.len()) {
        let &Entry { key: ref key_, ref data } = bucket.get(i);
        if key_ == key {
            return Some(data.clone())
        }
    }
    None
}

#[cfg(version3)]
pub fn find<A:Eq+Hash,B:Clone>(t: &Table<A,B>, key: &A) -> Option<B> {
    let bucket : &Vec<Entry<A,B>> = t.buckets.get(hash_bucket(key));
    for &Entry { key: ref key_, ref data } in bucket.iter() {
        if key_ == key { return Some(data.clone()) }
    }
    None
}

#[cfg(version4)]
pub fn find<A:Eq+Hash,B:Clone>(t: &Table<A,B>, key: &A) -> Option<B> {
    let bucket : &Vec<Entry<A,B>> = t.buckets.get(hash_bucket(key));

    bucket.iter()
        .find(|& &Entry {key: ref key_, ..}| key_ == key)
        .map(|&Entry{ ref data, .. }| data.clone())
}

pub fn iter<A, B>(t: &Table<A,B>, f: |&A, &B|) {
    for i in range(0, t.buckets.len()) {
        for entry in t.buckets.get(i).iter() {
            f(&entry.key, &entry.data)
        }
    }
}

fn bucket_has_key<A:Eq,B>(t: &mut Table<A,B>, i: uint, key: &A) -> bool {
    t.buckets.get(i).iter().any(|&Entry{ key: ref key_, .. }| key_ == key)
}

pub fn add<A:Eq+Hash, B>(t: &mut Table<A,B>, entry: Entry<A,B>) {
    let Entry{ key, data } = entry;
    let i = hash_bucket(&key);
    let replace = bucket_has_key(t,i,&key);
    let filtered_bucket =
        if replace {
            let bucket = t.buckets.get_mut(i);
            bucket.retain(|&Entry{ key: ref key_, .. }| key_ != &key);
            bucket
        } else {
            t.buckets.get_mut(i)
        };
    filtered_bucket.push(Entry{ key: key, data: data });
    if !replace {
        t.length += 1;
    }
}

pub fn remove<A:Eq+Hash,B>(t: &mut Table<A,B>, key: &A) {
    let i = hash_bucket(key);
    if bucket_has_key(t, i, key) {
        let bucket = t.buckets.get_mut(i);
        bucket.retain(|&Entry{ key: ref key_, .. }| key_ != key);
        t.length = t.length - 1;
    }
}

#[cfg(test)]
mod tests {
    use super::{Entry, Table};
    use super::{create, length, add, find, remove};

    static NUMS: [(&'static str, int), ..11] =
        [( "zero",0), ( "one",1), ( "two",2), ("three",3),
         ( "four",4), ("five",5), ( "six",6), ("seven",7),
         ("eight",8), ("nine",9), ( "ten",10)];

    fn create_zero_to_ten() -> Table<&'static str, int> {
        let mut t = create();
        for &(k, d) in NUMS.as_slice().iter() {
            add(&mut t, Entry{ key: k, data: d });
        }
        t
    }

    #[test]
    fn length_matches() {
        let t = create_zero_to_ten();
        assert!(length(&t) == 11);
    }

    #[test]
    fn lookup_zero_six_ten() {
        let t = create_zero_to_ten();
        assert!(find(&t, &"zero") == Some(0));
        assert!(find(&t, &"six")  == Some(6));
        assert!(find(&t, &"ten")  == Some(10));
    }

    #[test]
    fn remove_six_lookup_zero_six_ten() {
        let mut t = create_zero_to_ten();
        assert!(length(&t) == 11);
        remove(&mut t, &"six");
        assert!(length(&t) == 10);
        assert!(find(&t, &"zero") == Some(0));
        assert!(find(&t, &"six")  == None);
        assert!(find(&t, &"ten")  == Some(10));
        remove(&mut t, &"six");
        assert!(length(&t) == 10);
    }

    #[test]
    fn replace() {
        let mut t = create_zero_to_ten();
        for &(k, d) in NUMS.as_slice().tail().iter() {
            add(&mut t, Entry{ key: k, data: -d });
        }
        assert!(length(&t) == 11);
        for &(k, d) in NUMS.as_slice().iter() {
            assert!(find(&t, &k) == Some(-d));
        }
    }

}
