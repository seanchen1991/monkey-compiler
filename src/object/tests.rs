use object::*;
use std::collections::hash_map::DefaultHasher;

#[test]
fn string_hash_key() {
    let hello1 = Object::Str(String::from("Hello World"));
    let hello2 = Object::Str(String::from("Hello World"));
    let diff1 = Object::Str(String::from("my name is johnny"));
    let diff2 = Object::Str(String::from("my name is johnny"));

    let mut hasher1 = DefaultHasher::new();
    hello1.hash(&mut hasher1);
    
    let mut hasher2 = DefaultHasher::new();
    hello2.hash(&mut hasher2);

    assert_eq!(hasher1.finish(), hasher2.finish());

    let mut hasher1 = DefaultHasher::new();
    diff1.hash(&mut hasher1);

    let mut hasher2 = DefaultHasher::new();
    diff2.hash(&mut hasher2);

    assert_eq!(hasher1.finish(), hasher2.finish());

    let mut hasher1 = DefaultHasher::new();
    hello1.hash(&mut hasher1);

    let mut hasher2 = DefaultHasher::new();
    diff1.hash(&mut hasher2);

    assert_ne!(hasher1.finish(), hasher2.finish());
}