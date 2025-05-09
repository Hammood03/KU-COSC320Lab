// In this exercise, you'll learn some of the unique advantages that iterators
// can offer.

// TODO: Complete the `capitalize_first` function.
// "hello" -> "Hello"
fn capitalize_first(input: &str) -> String {
    let mut chars = input.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().to_string() + chars.as_str()
        
    }
}

// TODO: Apply the `capitalize_first` function to a slice of string slices.
// Return a vector of strings.
// ["hello", "world"] -> ["Hello", "World"]
fn capitalize_words_vector(words: &[&str]) -> Vec<String> {
    // ???
    let v = words.iter();
    let mut v1: Vec<String>=Vec::new();
    for s in v {
        let cap = capitalize_first(s);
        v1.push(cap.to_string());
    }
    return v1;
}

// TODO: Apply the `capitalize_first` function again to a slice of string
// slices. Return a single string.
// ["hello", " ", "world"] -> "Hello World"
fn capitalize_words_string(words: &[&str]) -> String {
    // ???
    let v = words.iter();
    let mut s: String = String::new();
    for s1 in v {
        let cap = capitalize_first(s1);
        s = s + &cap;
    }
    return s;
}

fn main() {
    // You can optionally experiment here.
    println!("hello: {}", capitalize_first("hello"));
    let words = vec!["hello", "world"];
    println!("hello, world : {:?}",capitalize_words_vector(&words));
    let words1 = vec!["hello", " ", "world"];
    println!("String: {}",capitalize_words_string(&words1));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_success() {
        assert_eq!(capitalize_first("hello"), "Hello");
    }

    #[test]
    fn test_empty() {
        assert_eq!(capitalize_first(""), "");
    }

    #[test]
    fn test_iterate_string_vec() {
        let words = vec!["hello", "world"];
        assert_eq!(capitalize_words_vector(&words), ["Hello", "World"]);
    }

    #[test]
    fn test_iterate_into_string() {
        let words = vec!["hello", " ", "world"];
        assert_eq!(capitalize_words_string(&words), "Hello World");
    }
}