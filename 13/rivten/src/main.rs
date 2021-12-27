#[derive(Debug, Copy, Clone)]
enum Fold {
    X(i32),
    Y(i32),
}

fn do_fold(points: &mut Vec<(i32, i32)>, fold: Fold) {
    match fold {
        Fold::X(fold_x) => {
            for (x, _) in points.iter_mut() {
                if *x > fold_x {
                    *x = 2 * fold_x - *x;
                }
            }
        }
        Fold::Y(fold_y) => {
            for (_, y) in points.iter_mut() {
                if *y > fold_y {
                    *y = 2 * fold_y - *y;
                }
            }
        }
    }
    points.sort();
    points.dedup();
}

fn main() {
    let file_content = std::fs::read_to_string("input.txt").unwrap();
    let mut paragraphs = file_content.split("\n\n");
    let first_part = paragraphs.next().unwrap();
    let second_part = paragraphs.next().unwrap();

    let mut points = Vec::<(i32, i32)>::new();
    let mut folds = Vec::new();

    for line in first_part.lines() {
        let mut line_splitted = line.split(',');
        let x = line_splitted.next().unwrap().parse().unwrap();
        let y = line_splitted.next().unwrap().parse().unwrap();
        points.push((x, y));
    }

    for line in second_part.lines() {
        let mut line_splitted = line.split('=');
        let axis = line_splitted.next().unwrap().chars().last().unwrap();
        let n = line_splitted.next().unwrap().parse().unwrap();
        let fold = match axis {
            'x' => Fold::X(n),
            'y' => Fold::Y(n),
            _ => panic!(),
        };
        folds.push(fold);
    }

    for fold in folds {
        do_fold(&mut points, fold);
    }

    let min_x = *points.iter().map(|(x, _)| x).min().unwrap();
    let max_x = *points.iter().map(|(x, _)| x).max().unwrap();

    let min_y = *points.iter().map(|(_, y)| y).min().unwrap();
    let max_y = *points.iter().map(|(_, y)| y).max().unwrap();

    for y in min_y..(max_y + 1) {
        for x in min_x..(max_x + 1) {
            if points.contains(&(x, y)) {
                print!("#");
            } else {
                print!(".");
            }
        }
        print!("\n");
    }
}
