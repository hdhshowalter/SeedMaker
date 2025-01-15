
test_that("seed_gen generates a vector of seeds from a single seed", {

  n <- 10

  seeds1 <- seed_gen(
    seed = 1234,
    n = n
  )

  expect_type(seeds1, "integer")
  expect_length(seeds1, n)
  expect_true(all(1 <= seeds1 & seeds1 <= 1000000))

  seeds2 <- seed_gen(
    seed = 1234,
    n = 10,
    sample_vector = 1:100
  )

  expect_type(seeds2, "integer")
  expect_length(seeds2, n)
  expect_true(all(1 <= seeds2 & seeds2 <= 100))

  seeds3 <- seed_gen(
    seed = 5678,
    n = 10
  )

  expect_type(seeds3, "integer")
  expect_length(seeds3, n)
  expect_true(all(1 <= seeds3 & seeds3 <= 1000000))
  expect_false(identical(seeds1, seeds3))

  n_alt <- 1000

  seeds4 <- seed_gen(
    seed = 1234,
    n = n_alt
  )

  expect_type(seeds4, "integer")
  expect_length(seeds4, n_alt)
  expect_true(all(1 <= seeds4 & seeds4 <= 1000000))
})

test_that("seed_gen fails correctly", {

  expect_warning(expect_error(seed_gen(
    seed = "a",
    n = 10
  )))
  expect_error(seed_gen(
    n = 10
  ))
  expect_error(seed_gen(
    seed = 1234
  ))
  expect_error(seed_gen(
    seed = 1234,
    n = 0
  ))
  expect_error(seed_gen(
    seed = 1234,
    n = 1.5
  ))
  expect_error(seed_gen(
    seed = 1234,
    n = "a"
  ))
  expect_error(seed_gen(
    seed = 1234,
    n = 10,
    sample_vector = -1:1000000
  ))
  expect_error(seed_gen(
    seed = 1234,
    n = 10,
    sample_vector = 1:9
  ))
  expect_error(seed_gen(
    seed = 1234,
    n = 10,
    sample_vector = c(1:10, NA)
  ))
  expect_error(seed_gen(
    seed = 1234,
    n = 10,
    sample_vector = c(1:10, 10)
  ))
})

test_that("seed_maker constructs a hierarchical list containing a collection of seeds from a single seed.", { # nolint

  n_per_level1 <- 10
  level_names1 <- "sim"

  seeds_list1 <- seed_maker(
    seed = 1234,
    level_names = level_names1,
    n_per_level = n_per_level1
  )

  expect_type(seeds_list1, "list")
  expect_length(seeds_list1, n_per_level1)
  expect_named(
    seeds_list1,
    paste0(level_names1, 1:n_per_level1)
  )
  for (i in names(seeds_list1)) {
    expect_true(1 <= seeds_list1[[i]] & seeds_list1[[i]] <= 1000000)
  }

  n_per_level2 <- 15
  level_names2 <- "dataset"

  seeds_list2 <- seed_maker(
    seed = 1234,
    level_names = c(level_names1, level_names2),
    n_per_level = c(n_per_level1, n_per_level2)
  )

  expect_type(seeds_list2, "list")
  expect_length(seeds_list2, n_per_level1)
  expect_named(
    seeds_list2,
    paste0(level_names1, 1:n_per_level1)
  )
  for (i in names(seeds_list2)) {
    expect_type(seeds_list2[[i]], "list")
    expect_length(seeds_list2[[i]], n_per_level2)
    expect_named(
      seeds_list2[[i]],
      paste0(level_names2, 1:n_per_level2)
    )
    for (j in names(seeds_list2[[i]])) {
      expect_true(1 <= seeds_list2[[i]][[j]] & seeds_list2[[i]][[j]] <= 1000000)
    }
  }

  n_per_level3 <- 3
  level_names3 <- c("component_a", "component_b", "component_c")

  seeds_list3 <- seed_maker(
    seed = 1234,
    level_names = list(level_names3),
    n_per_level = n_per_level3
  )

  expect_type(seeds_list3, "list")
  expect_length(seeds_list3, n_per_level3)
  expect_named(
    seeds_list3,
    level_names3
  )
  for (i in names(seeds_list3)) {
    expect_true(1 <= seeds_list3[[i]] & seeds_list3[[i]] <= 1000000)
  }

  seeds_list4 <- seed_maker(
    seed = 1234,
    level_names = list(level_names1, level_names3),
    n_per_level = c(n_per_level1, n_per_level3)
  )

  expect_type(seeds_list4, "list")
  expect_length(seeds_list4, n_per_level1)
  expect_named(
    seeds_list4,
    paste0(level_names1, 1:n_per_level1)
  )
  for (i in names(seeds_list4)) {
    expect_type(seeds_list4[[i]], "list")
    expect_length(seeds_list4[[i]], n_per_level3)
    expect_named(
      seeds_list4[[i]],
      level_names3
    )
    for (j in names(seeds_list4[[i]])) {
      expect_true(1 <= seeds_list4[[i]][[j]] & seeds_list4[[i]][[j]] <= 1000000)
    }
  }

  seeds_list5 <- seed_maker(
    seed = 1234,
    level_names = list(level_names3, level_names1),
    n_per_level = c(n_per_level3, n_per_level1)
  )

  expect_type(seeds_list5, "list")
  expect_length(seeds_list5, n_per_level3)
  expect_named(
    seeds_list5,
    level_names3
  )
  for (i in names(seeds_list5)) {
    expect_type(seeds_list5[[i]], "list")
    expect_length(seeds_list5[[i]], n_per_level1)
    expect_named(
      seeds_list5[[i]],
      paste0(level_names1, 1:n_per_level1)
    )
    for (j in names(seeds_list5[[i]])) {
      expect_true(1 <= seeds_list5[[i]][[j]] & seeds_list5[[i]][[j]] <= 1000000)
    }
  }

  seeds_list6 <- seed_maker(
    seed = 1234,
    level_names = level_names1,
    n_per_level = n_per_level1,
    sample_vector = list(1:100)
  )

  expect_type(seeds_list6, "list")
  expect_length(seeds_list6, n_per_level1)
  expect_named(
    seeds_list6,
    paste0(level_names1, 1:n_per_level1)
  )
  for (i in names(seeds_list6)) {
    expect_true(1 <= seeds_list6[[i]] & seeds_list6[[i]] <= 100)
  }

  seeds_list7 <- seed_maker(
    seed = 1234,
    level_names = level_names1,
    n_per_level = 1,
    index_level = FALSE
  )

  expect_type(seeds_list7, "list")
  expect_length(seeds_list7, 1)
  expect_named(
    seeds_list7,
    level_names1
  )
  for (i in names(seeds_list7)) {
    expect_true(1 <= seeds_list7[[i]] & seeds_list7[[i]] <= 1000000)
  }

  seeds_list8 <- seed_maker(
    seed = 1234,
    level_names = list(level_names1, level_names3),
    n_per_level = c(1, n_per_level3),
    index_level = c(FALSE, FALSE)
  )

  expect_type(seeds_list8, "list")
  expect_length(seeds_list8, 1)
  expect_named(
    seeds_list8,
    level_names1
  )
  for (i in names(seeds_list8)) {
    expect_type(seeds_list8[[i]], "list")
    expect_length(seeds_list8[[i]], n_per_level3)
    expect_named(
      seeds_list8[[i]],
      level_names3
    )
    for (j in names(seeds_list8[[i]])) {
      expect_true(1 <= seeds_list8[[i]][[j]] & seeds_list8[[i]][[j]] <= 1000000)
    }
  }

  seeds_list9 <- seed_maker(
    seed = 1234,
    level_names = list(level_names3, level_names1),
    n_per_level = c(3, n_per_level1),
    index_level = c(FALSE, TRUE)
  )

  expect_type(seeds_list9, "list")
  expect_length(seeds_list9, n_per_level3)
  expect_named(
    seeds_list9,
    level_names3
  )
  for (i in names(seeds_list9)) {
    expect_type(seeds_list9[[i]], "list")
    expect_length(seeds_list9[[i]], n_per_level1)
    expect_named(
      seeds_list9[[i]],
      paste0(level_names1, 1:n_per_level1)
    )
    for (j in names(seeds_list9[[i]])) {
      expect_true(1 <= seeds_list9[[i]][[j]] & seeds_list9[[i]][[j]] <= 1000000)
    }
  }

  seeds_list10 <- seed_maker(
    seed = 1234,
    level_names = list(level_names3, level_names1),
    n_per_level = c(3, 1),
    index_level = c(FALSE, FALSE)
  )

  expect_type(seeds_list10, "list")
  expect_length(seeds_list10, n_per_level3)
  expect_named(
    seeds_list10,
    level_names3
  )
  for (i in names(seeds_list10)) {
    expect_type(seeds_list10[[i]], "list")
    expect_length(seeds_list10[[i]], 1)
    expect_named(
      seeds_list10[[i]],
      level_names1
    )
    for (j in names(seeds_list10[[i]])) {
      expect_true(
        1 <= seeds_list10[[i]][[j]] & seeds_list10[[i]][[j]] <= 1000000
      )
    }
  }

  seeds_list11 <- seed_maker(
    seed = 1234,
    level_names = list(level_names1, level_names2, level_names3),
    n_per_level = c(n_per_level1, n_per_level2, n_per_level3)
  )

  expect_type(seeds_list11, "list")
  expect_length(seeds_list11, n_per_level1)
  expect_named(
    seeds_list11,
    paste0(level_names1, 1:n_per_level1)
  )
  for (i in names(seeds_list11)) {
    expect_type(seeds_list11[[i]], "list")
    expect_length(seeds_list11[[i]], n_per_level2)
    expect_named(
      seeds_list11[[i]],
      paste0(level_names2, 1:n_per_level2)
    )
    for (j in names(seeds_list11[[i]])) {
      expect_type(seeds_list11[[i]][[j]], "list")
      expect_length(seeds_list11[[i]][[j]], n_per_level3)
      expect_named(
        seeds_list11[[i]][[j]],
        level_names3
      )
      for (k in names(seeds_list11[[i]][[j]])) {
        expect_true(
          1 <= seeds_list11[[i]][[j]][[k]] &
            seeds_list11[[i]][[j]][[k]] <= 1000000
        )
      }
    }
  }

  seeds_list12 <- seed_maker(
    seed = 1234,
    level_names = list(level_names1, level_names3, level_names2),
    n_per_level = c(n_per_level1, n_per_level3, n_per_level2)
  )

  expect_type(seeds_list12, "list")
  expect_length(seeds_list12, n_per_level1)
  expect_named(
    seeds_list12,
    paste0(level_names1, 1:n_per_level1)
  )
  for (i in names(seeds_list12)) {
    expect_type(seeds_list12[[i]], "list")
    expect_length(seeds_list12[[i]], n_per_level3)
    expect_named(
      seeds_list12[[i]],
      level_names3
    )
    for (j in names(seeds_list12[[i]])) {
      expect_type(seeds_list12[[i]][[j]], "list")
      expect_length(seeds_list12[[i]][[j]], n_per_level2)
      expect_named(
        seeds_list12[[i]][[j]],
        paste0(level_names2, 1:n_per_level2)
      )
      for (k in names(seeds_list12[[i]][[j]])) {
        expect_true(
          1 <= seeds_list12[[i]][[j]][[k]] &
            seeds_list12[[i]][[j]][[k]] <= 1000000
        )
      }
    }
  }

  seeds_flat1 <- seed_maker(
    seed = 1234,
    level_names = level_names1,
    n_per_level = n_per_level1,
    flatten = TRUE
  )

  expect_type(seeds_flat1, "integer")
  expect_length(seeds_flat1, n_per_level1)
  expect_named(
    seeds_flat1,
    paste0(level_names1, 1:n_per_level1)
  )
  for (i in names(seeds_flat1)) {
    expect_true(1 <= seeds_flat1[[i]] & seeds_flat1[[i]] <= 1000000)
  }

  seeds_flat2 <- seed_maker(
    seed = 1234,
    level_names = c(level_names1, level_names2),
    n_per_level = c(n_per_level1, n_per_level2),
    flatten = TRUE
  )

  expect_type(seeds_flat2, "list")
  expect_length(seeds_flat2, n_per_level1)
  expect_named(
    seeds_flat2,
    paste0(level_names1, 1:n_per_level1)
  )
  for (i in names(seeds_flat2)) {
    expect_type(seeds_flat2[[i]], "integer")
    expect_length(seeds_flat2[[i]], n_per_level2)
    expect_named(
      seeds_flat2[[i]],
      paste0(level_names2, 1:n_per_level2)
    )
    for (j in names(seeds_flat2[[i]])) {
      expect_true(1 <= seeds_flat2[[i]][[j]] & seeds_flat2[[i]][[j]] <= 1000000)
    }
  }

  seeds_flat3 <- seed_maker(
    seed = 1234,
    level_names = list(level_names1, level_names3),
    n_per_level = c(n_per_level1, n_per_level3),
    flatten = TRUE
  )

  expect_type(seeds_flat3, "list")
  expect_length(seeds_flat3, n_per_level1)
  expect_named(
    seeds_flat3,
    paste0(level_names1, 1:n_per_level1)
  )
  for (i in names(seeds_flat3)) {
    expect_type(seeds_flat3[[i]], "integer")
    expect_length(seeds_flat3[[i]], n_per_level3)
    expect_named(
      seeds_flat3[[i]],
      level_names3
    )
    for (j in names(seeds_flat3[[i]])) {
      expect_true(1 <= seeds_flat3[[i]][[j]] & seeds_flat3[[i]][[j]] <= 1000000)
    }
  }

  seeds_flat4 <- seed_maker(
    seed = 1234,
    level_names = level_names1,
    n_per_level = n_per_level1,
    sample_vector = list(1:100),
    flatten = TRUE
  )

  expect_type(seeds_flat4, "integer")
  expect_length(seeds_flat4, n_per_level1)
  expect_named(
    seeds_flat4,
    paste0(level_names1, 1:n_per_level1)
  )
  for (i in names(seeds_flat4)) {
    expect_true(1 <= seeds_flat4[[i]] & seeds_flat4[[i]] <= 100)
  }

  seeds_flat5 <- seed_maker(
    seed = 1234,
    level_names = level_names1,
    n_per_level = 1,
    index_level = FALSE,
    flatten = TRUE
  )

  expect_type(seeds_flat5, "integer")
  expect_length(seeds_flat5, 1)
  expect_named(
    seeds_flat5,
    level_names1
  )
  for (i in names(seeds_flat5)) {
    expect_true(1 <= seeds_flat5[[i]] & seeds_flat5[[i]] <= 1000000)
  }

  seeds_flat6 <- seed_maker(
    seed = 1234,
    level_names = list(level_names1, level_names3),
    n_per_level = c(1, n_per_level3),
    index_level = c(FALSE, FALSE),
    flatten = TRUE
  )

  expect_type(seeds_flat6, "list")
  expect_length(seeds_flat6, 1)
  expect_named(
    seeds_flat6,
    level_names1
  )
  for (i in names(seeds_flat6)) {
    expect_type(seeds_flat6[[i]], "integer")
    expect_length(seeds_flat6[[i]], n_per_level3)
    expect_named(
      seeds_flat6[[i]],
      level_names3
    )
    for (j in names(seeds_flat6[[i]])) {
      expect_true(1 <= seeds_flat6[[i]][[j]] & seeds_flat6[[i]][[j]] <= 1000000)
    }
  }
})

test_that("seed_maker fails correctly", {

  expect_error(seed_maker(
    seed = 1234,
    n_per_level = 10
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = 1,
    n_per_level = 10
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = character(),
    n_per_level = 10
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = c("sim", NA),
    n_per_level = 10
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = list("sim", 1),
    n_per_level = c(10, 3)
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = list("sim", NULL),
    n_per_level = c(10, 3)
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = list(),
    n_per_level = c(10, 3)
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = list("sim", NA),
    n_per_level = c(10, 3)
  ))

  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim"
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 0
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 1.5
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = "a"
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = integer()
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = c(10L, NA)
  ))

  expect_error(seed_maker(
    seed = 1234,
    level_names = list("sim", c("component_a", "component_b", NA)),
    n_per_level = c(10, 3)
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = list("sim", c("component_a", "component_b")),
    n_per_level = c(10, 3)
  ))

  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    sample_vector = 1
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    sample_vector = "a"
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    sample_vector = list(NULL)
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    sample_vector = list()
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    sample_vector = list(1:1000000, 1:1000000)
  ))

  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    index_level = 1
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    index_level = "a"
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    index_level = NA
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    index_level = logical()
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    index_level = c(TRUE, TRUE)
  ))

  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    flatten = 1
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    flatten = "a"
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    flatten = NA
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    flatten = logical()
  ))
  expect_error(seed_maker(
    seed = 1234,
    level_names = "sim",
    n_per_level = 10,
    flatten = c(TRUE, TRUE)
  ))

  expect_error(seed_maker(
    seed = 1234,
    level_names = c("sim"),
    n_per_level = 10,
    index_level = FALSE
  ))
})
