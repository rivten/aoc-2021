use std::io::BufRead;

struct PathIterator<'a> {
    graph: &'a std::collections::HashMap<String, Vec<String>>,
    stack: Vec<(Vec<String>, Vec<String>)>,
}

impl<'a> PathIterator<'a> {
    fn new(graph: &'a std::collections::HashMap<String, Vec<String>>) -> Self {
        Self {
            graph,
            stack: vec![(
                vec!["start".to_string()],
                graph.get("start").unwrap().clone(),
            )],
        }
    }
}

impl std::iter::Iterator for PathIterator<'_> {
    type Item = Vec<String>;

    fn next(&mut self) -> Option<Self::Item> {
        let (next_path_to_evaluate, next_possible_vertices) = self.stack.pop()?;
        //println!("{:?} {:?}", next_path_to_evaluate, next_possible_vertices);
        for v in next_possible_vertices {
            let is_small_cave = v.chars().next().unwrap().is_ascii_lowercase();
            if is_small_cave && next_path_to_evaluate.contains(&v) {
                continue;
            }
            let mut to_push = next_path_to_evaluate.clone();
            to_push.push(v.clone());
            if let Some(next_vertices) = self.graph.get(&v) {
                self.stack.push((to_push, next_vertices.clone()));
            } else {
                self.stack.push((to_push, vec![]));
            }
        }

        Some(next_path_to_evaluate)
    }
}

fn main() {
    let file_handle = std::fs::File::open("sample.txt").unwrap();
    let reader = std::io::BufReader::new(file_handle);

    let mut lines = reader.lines();

    let mut graph = std::collections::HashMap::<String, Vec<String>>::new();

    while let Some(line) = lines.next() {
        let line = line.unwrap().clone();
        let mut splitted_line = line.split("-");

        let start = splitted_line.next().unwrap();
        let end = splitted_line.next().unwrap();

        graph
            .entry(start.to_string())
            .or_insert(Vec::<String>::new())
            .push(end.to_string());

        if end != "end" {
            graph
                .entry(end.to_string())
                .or_insert(Vec::<String>::new())
                .push(start.to_string());
        }
    }

    let path_iterator = PathIterator::new(&graph);
    println!(
        "{}",
        path_iterator
            .filter(|path| path.iter().last().unwrap() == "end")
            .count()
    );
}
