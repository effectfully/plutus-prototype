fn main() {
    println!("{}", miden_crypto::Felt::inner(&miden_crypto::Felt::new(4)));

    println!("{:?}", ([1,2,3,4].map(|x| miden_crypto::Felt::new(x))));

    println!("{:?}", miden_crypto::hash::rpo::RpoDigest::as_elements(&miden_crypto::hash::rpo::RpoDigest::new([1,2,3,4].map(|x| miden_crypto::Felt::new(x)))));

    println!("{}", miden_crypto::hash::rpo::RpoDigest::as_elements(&miden_crypto::hash::rpo::RpoDigest::new([1,2,3,4].map(|x| miden_crypto::Felt::new(x)))).iter().map(|x| miden_crypto::Felt::inner(x)).collect::<Vec<u64>>().iter().fold(String::new(), |acc, &el| acc + &el.to_string() + ", "));

    println!("{:?}", miden_crypto::hash::rpo::RpoDigest::as_elements(&miden_crypto::hash::rpo::Rpo256::hash_elements(&[1,2,3,4,5,6,7,8].map(|x| miden_crypto::Felt::new(x)))));

    println!("{}", miden_crypto::hash::rpo::RpoDigest::as_elements(&miden_crypto::hash::rpo::RpoDigest::new([1,2,3,4].map(|x| miden_crypto::Felt::new(x)))).iter().fold(String::new(), |acc, &el| acc + &el.to_string() + ", "));

    println!("{}", miden_crypto::hash::rpo::RpoDigest::as_elements(&miden_crypto::hash::rpo::Rpo256::hash_elements(&[1,2,3,4,5,6,7,8].map(|x| miden_crypto::Felt::new(x)))).iter().fold(String::new(), |acc, &el| acc + &el.to_string() + ", "));
}
