#[allow(unused_mut)]; // workaround brokenness here.

fn is_digit(c:char) -> bool {
    let zero = '0' as uint;
    let nine = '9' as uint;
    let i = c as uint;
    return zero <= i && i <= nine;
}

fn find_numeric_position(msg: &str, offset:uint) -> Option<(uint, uint)> {
    let suffix = msg.slice_from(offset);
    let start = suffix.find(is_digit);
    let is_not_digit = |c| ! is_digit(c);
    match start {
        None => None,
        Some(where) => {
            let last = suffix.slice_from(where).find(is_not_digit);
            let limit = where + last.unwrap_or(1);
            let extent = (offset+where, offset+limit);
            return Some(extent);
        }
    }
}

fn all_numeric_positions(msg: &str) -> ~[(uint, uint)] {
    let mut p = recur(msg, 0);
    p.reverse();
    return p;

    fn recur(msg:&str, offset:uint) -> ~[(uint, uint)] {
        let first = find_numeric_position(msg, offset);
        debug!("all_numeric_positions recur {:?} {} first: {:?}",
               msg, offset, first);
        match first {
            None => return ~[],
            Some(p) => {
                let (_, limit) = p;
                let mut rest = recur(msg, limit);
                rest.push(p);
                return rest;
            }
        }
    }
}

#[cfg(entropy)]
fn drink(mut bytes: &mut [u8]) {
    use std::rand;
    let (idx, offset, heads) = rand::random::<(uint, uint, bool)>();
    let idx = idx % bytes.len();
    let _offset = offset % 8;
    let b = bytes[idx];
    // let b = b ^ (1 << offset);
    let b = if heads { b + 10 } else { b - 10 };
    bytes[idx] = b;
}

#[cfg(not(entropy))]
fn drink(_bytes: &mut [u8]) {
    // no-op
}

fn remove_a_bottle(msg: ~str) -> ~str {
    use std::str;
    use std::int;
    let radix = 10;
    let spots = all_numeric_positions(msg);
    let mut bytes = msg.into_bytes();
    let mut found_one = false;
    for &(start,limit) in spots.iter() {
        let c = int::parse_bytes(bytes.slice(start, limit), radix);
        let c = c.unwrap(); // fail!'s if None.
        if c == 0 {
            continue; // don't decrement a number if its already zero.
        }
        found_one = true;
        let c = c - 1;
        int::to_str_bytes(c, radix, |v| {
                for i in range(0, limit-start) {
                    if i < v.len() {
                        bytes[start + i] = v[i];
                    } else {
                        bytes[start + i] = ' ' as u8;
                    }
                }
            });
    }
    if found_one { drink(bytes); }
    return str::from_utf8_owned(bytes);
}

fn main() {
    use std::io::timer;

    let n = 13;

    let msg_beer = ~"99 bottles of beer on the wall, 99 bottles of beer. \
                     Take one down, pass it around, \
                     99 bottles of beer on the wall.";
    println!("The numbers are at: {:?}", all_numeric_positions(msg_beer));
    let split : ~[&str] = msg_beer.split(|c:char|c.is_digit()).collect();
    println!("Split by digits: {:?}", split);

    let mut seq_nums = range(0, n);
    // (remember there is also comm::oneshot streams.  may be useful.)

    let (init_recv_port, init_send_chan) = stream::<~str>();

    let last_recv_port = seq_nums.fold(
        init_recv_port,
        |prev_port, seq_num| -> Port<~str> {
            let (recv_port, send_chan) = stream::<~str>();
            do spawn {
                let my_seq_num   = seq_num;
                let my_send_chan = send_chan;
                let my_recv_port = prev_port;

                loop {
                    let msg = my_recv_port.recv();
                    println!("Task {} received {:?}", my_seq_num, msg);
                    let msg = remove_a_bottle(msg);
                    my_send_chan.send(msg);
                }
            }
            recv_port
        });

    init_send_chan.send(msg_beer);
    let silly = ~"did you get my text?";
    let silly2 = silly.clone();
    let silly = silly2;
    timer::sleep(1);
    init_send_chan.send(silly.clone());
    loop {
        let msg = last_recv_port.recv();

        println!("Main task received {:?}", msg);
        let first_num = find_numeric_position(msg, 0);
        match first_num {
            None => {},
            Some((start, limit)) => {
                let n : Option<int> = FromStr::from_str(msg.slice(start, limit));
                match n {
                    None => fail!("find_numeric_position broken"),
                    Some(n) if n == 0 => {
                        println!("Closing down in response to {:?}", msg);
                        break;
                    }
                    Some(_other) => {}
                }
            }
        }

        init_send_chan.send(msg);
    }

    loop {} // diverge rather than fail scarily at end.
}
