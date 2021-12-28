fn parse_file_content(file_content: &str) -> (i32, i32, i32, i32) {
    let (_, d) = file_content.split_once(':').unwrap();
    let (xs, ys) = d.split_once(',').unwrap();
    let (_, dx) = xs.split_once('=').unwrap();
    let (_, dy) = ys.split_once('=').unwrap();

    let (min_x, max_x) = dx.split_once("..").unwrap();
    let (min_y, max_y) = dy.split_once("..").unwrap();

    (
        min_x.trim().parse().unwrap(),
        max_x.trim().parse().unwrap(),
        min_y.trim().parse().unwrap(),
        max_y.trim().parse().unwrap(),
    )
}

fn pos(vx0: i32, vy0: i32, n: i32) -> (i32, i32) {
    assert!(n >= 1);

    let yn = n * vy0 - (n * (n - 1)) / 2;
    let xn = if vx0 > 0 {
        if n <= vx0 {
            n * vx0 - (n * (n - 1)) / 2
        } else {
            vx0 * vx0 - (vx0 * (vx0 - 1)) / 2
        }
    } else {
        if n <= -vx0 {
            n * vx0 + (n * (n - 1)) / 2
        } else {
            vx0 * vx0 + (vx0 * (vx0 - 1)) / 2
        }
    };

    (xn, yn)
}

fn main() {
    //let file_content = std::fs::read_to_string("sample.txt").unwrap();
    let file_content = std::fs::read_to_string("input.txt").unwrap();
    let (target_min_x, target_max_x, target_min_y, target_max_y) =
        parse_file_content(&file_content);

    println!(
        "{:?}",
        (target_min_x, target_max_x, target_min_y, target_max_y)
    );

    assert!(target_min_x > 0);
    assert!(target_max_x > target_min_x);
    assert!(target_min_y < 0);
    assert!(target_min_y < target_max_y);

    // For vy0 >= 0, we know that the Y coordinates has two roots
    // One at step n = 0 (logic)
    // And one at step n = 1 + 2 * vy0
    // So we know that the y coordinate at step n = 2 + 2 * vy0 is
    // - 1 - vy0
    // If this is smaller that the target_min_y, we are not going to hit

    let max_vx0_to_test = target_max_x + 1;
    let min_vy0_to_test = target_min_y - 1;
    let mut highest_y = 0;
    let mut valid_shot_count = 0;
    for vx0 in 0..(max_vx0_to_test + 1) {
        println!("Setting vx0 = {}", vx0);
        let max_vy0_to_test = std::cmp::max(vx0, -target_min_y - 1);
        for vy0 in min_vy0_to_test..(max_vy0_to_test + 1) {
            println!("Doing shot with vx0 = {} and vy0 = {}", vx0, vy0);
            let (mut last_x, mut last_y) = (0, 0);
            let mut is_valid_shot = false;
            let mut shot_highest_y = 0;
            let mut n = 1;
            while last_x < target_max_x && last_y > target_min_y {
                let (x, y) = pos(vx0, vy0, n);
                println!("Doing step {}, pos is ({}, {})", n, x, y);
                last_x = x;
                last_y = y;
                shot_highest_y = std::cmp::max(shot_highest_y, y);

                if x >= target_min_x && x <= target_max_x && y >= target_min_y && y <= target_max_y
                {
                    is_valid_shot = true;
                    println!("SHOT IS VALID");
                    valid_shot_count += 1;
                    break;
                }
                n += 1;
            }
            if is_valid_shot {
                highest_y = std::cmp::max(highest_y, shot_highest_y);
            }
        }
    }

    println!("{}", highest_y);
    println!("{}", valid_shot_count);
}
