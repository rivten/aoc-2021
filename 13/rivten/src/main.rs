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
        println!("{}", points.len());
    }

    for p in points {
        println!("{:?}", p);
    }
}
