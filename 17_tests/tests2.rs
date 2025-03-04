// Calculates the power of 2 using a bit shift.
// `1 << n` is equivalent to "2 to the power of n".
fn power_of_2(n: u8) -> u64 {
    1 << n
}

fn main() {
    // You can optionally experiment here.
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn you_can_assert_eq() {
        // TODO: Test the function `power_of_2` with some values.
        let a: u64=power_of_2(2);
        let b: u64=power_of_2(3);
        let c: u64=power_of_2(4);
        let d: u64=power_of_2(1);
        assert_eq!(a, 4);
        assert_eq!(b, 8);
        assert_eq!(c, 16);
        assert_eq!(d, 2);
    }
}