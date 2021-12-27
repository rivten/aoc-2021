use std::io::BufRead;

struct PathIterator<'a> {
    graph: &'a std::collections::HashMap<String, Vec<String>>,
    stack: Vec<(Vec<&'a str>, Vec<&'a str>, bool)>,
}

impl<'a> PathIterator<'a> {
    fn new(graph: &'a std::collections::HashMap<String, Vec<String>>) -> Self {
        Self {
            graph,
            stack: vec![(
                vec!["start"],
                graph
                    .get("start")
                    .unwrap()
                    .iter()
                    .map(String::as_str)
                    .collect(),
                false,
            )],
        }
    }
}

impl<'a> std::iter::Iterator for PathIterator<'a> {
    type Item = Vec<&'a str>;

    fn next(&mut self) -> Option<Self::Item> {
        let (next_path_to_evaluate, next_possible_vertices, single_small_cave_visited_twice) =
            self.stack.pop()?;
        //println!(
        //    "{:?} {:?} {:?}",
        //    next_path_to_evaluate, next_possible_vertices, single_small_cave_visited_twice
        //);
        for v in next_possible_vertices {
            let is_small_cave = v.chars().next().unwrap().is_ascii_lowercase();
            let visit_count = next_path_to_evaluate
                .iter()
                .filter(|&test_v| test_v == &v)
                .count();
            if is_small_cave && single_small_cave_visited_twice && visit_count >= 1 {
                continue;
            } else if is_small_cave && !single_small_cave_visited_twice && visit_count >= 2 {
                continue;
            }
            let mut to_push = next_path_to_evaluate.clone();
            to_push.push(v.clone());
            if let Some(next_vertices) = self.graph.get(v) {
                self.stack.push((
                    to_push,
                    next_vertices.iter().map(String::as_str).collect(),
                    single_small_cave_visited_twice || (is_small_cave && visit_count >= 1),
                ));
            } else {
                self.stack.push((
                    to_push,
                    vec![],
                    single_small_cave_visited_twice || (is_small_cave && visit_count >= 1),
                ));
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

        if start != "end" && end != "start" {
            graph
                .entry(start.to_string())
                .or_insert(Vec::<String>::new())
                .push(end.to_string());
        }

        if end != "end" && start != "start" {
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
            .filter(|path| path.iter().last().unwrap() == &"end")
            .count()
    );
}
