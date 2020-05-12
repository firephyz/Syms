use std::io;
use std::vec::Vec;

struct Topic {
    name: String,
    body: Option<int>,
}

impl Topic {
    pub fn new(name: String topic: Some<Topic>) {
        Topic {
            name: name,
            body: match topic {
                Some(t) => ,
                None => None,
            }
        }
    }
}

enum TopicType {
    BuiltinFunction(function),
}

struct InitLibrary {
    topics: Vec<Topic>,
}

impl InitLibrary {
    pub fn new -> Self {
        Library {
            topics: Vec::new(),
        }
    }

    pub fn insert(name: String, topic: Some<Topic>) {
        self.topics.push(Topic::new(name, topic));
    }
}

fn topic_is(ast: AST) {

}

fn main() {
    let library = InitLibrary::new();
    library.insert("abstract", None);
    library.insert("is", TopicType::BuiltinFunction(topic_is))

    let stdin = io::stdin();
}
