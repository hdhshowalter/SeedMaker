#' @title Generate a vector of seeds
#' @description `seed_gen()` generates a vector of seeds from a single seed.
#' @param seed A number, interpreted as an integer, or NULL (see
#'   [`set.seed()`][base::set.seed()]).
#' @param n The number of seeds to generate.
#' @param sample_vector The values from which to sample (without replacement).
#' @details
#'   `length(sample_vector)` must be >= `n`.
#'
#'   The values of `sample_vector` must be unique, positive integers.
#' @return
#'   An integer vector containing `n` randomly drawn values from
#'   `sample_vector`.
#' @examples
#' seeds1 <- seed_gen(
#'   seed = 1234,
#'   n = 10
#' )
#'
#' seeds2 <- seed_gen(
#'   seed = 1234,
#'   n = 10,
#'   sample_vector = 1:100
#' )
#' @export
seed_gen <- function(seed,
                     n,
                     sample_vector = 1:1000000) {

  assertIntegerish(n, lower = 1, any.missing = FALSE, len = 1)
  assertIntegerish(
    sample_vector,
    lower = 0,
    any.missing = FALSE,
    min.len = n,
    unique = TRUE
  )

  set.seed(seed)
  return(sample(sample_vector, n))
}

#' @title Construct a list containing a collection of seeds
#' @description
#'   `seed_maker()` constructs a hierarchical list containing a collection of
#'   seeds from a single seed.
#' @inheritParams seed_gen
#' @param level_names A character vector, or list of character vectors,
#'   indicating the element name(s) to use for each level of the returned list.
#' @param n_per_level An integer vector indicating the number of elements to
#'   produce at each level of the returned list.
#' @param sample_vector A list of expressions that are evaluable as integer
#'   vectors indicating the values from which to sample (without replacement) at
#'   each level of the returned list.
#' @param index_level A logical vector indicating whether to index the root name
#'   of a respective level.
#' @param flatten "Flatten" the lowest level of the returned list into a named
#'   vector?
#' @details
#'   `length(level_names)` must be equal to `length(n_per_level)` must be equal
#'   equal to `length(sample_vector)` must be equal to `length(index_level)`.
#'   When these arguments are greater than length 1, the intermediate seeds used
#'   to produce the seeds at the lowest level of the returned list are not
#'   included therein. In other words, only the collection of seeds at the
#'   lowest list level are returned.
#'
#'   If `level_names` is a character vector, each of its elements is used as the
#'   root name for the list elements produced at the respective level. See
#'   `seeds_list1` and `seeds_list2` in **Examples**.
#'
#'   If `level_names` is a list of character vectors:
#'   * The element contained in a length 1 vector is used as the root name for
#'   the list elements produced at the respective level.
#'   * Elements contained in a vector that is greater than length 1 are used
#'   verbatim as the names for the list elements produced at the respective
#'   level. The corresponding value of `n_per_level` must match the length of
#'   the vector.
#'
#'   See `seeds_list3`, `seeds_list4`, and `seeds_list5` in **Examples**.
#'
#'   For each element, i, of `sample_vector`:
#'   * `length(eval(sample_vector[[i]]))` must be >= `n_per_level[i]`.
#'   * The values of `eval(sample_vector[[i]])` must be unique, positive
#'   integers.
#'
#'   See `seeds_list6`, `seeds_list7`, and `seeds_list8` in **Examples**.
#'
#'   Element name indexing applies to root name-provided levels only. Thus, for
#'   each element, i, of `index_level`, if `length(level_names[[i]])` is greater
#'   than 1, the value of `index_level[i]` is ignored. Otherwise:
#'   * When `index_level[i]` is TRUE, the element names at the respective level
#'   are indexed from 1 - `n_per_level[i]`.
#'   * When `index_level[i]` is FALSE, the element name at the respective level
#'   is used verbatim, so long as `n_per_level[i]` = 1, since suppressing
#'   element name indexing makes sense only for single-element levels.
#'
#'   See `seeds_list9`, `seeds_list10`, `seeds_list11`, `seeds_list12`, and
#'   `seeds_list13` in **Examples**.
#'
#'   When `flatten` is set to TRUE, each set of seeds at the lowest level of the
#'   returned list is converted into a named vector (versus remaining in the
#'   default structure, which is a list of single value elements). When only a
#'   single level is specified (via `level_names`, `n_per_level`,
#'   `sample_vector`, and `index_level` all being length 1), a named integer
#'   vector is returned instead of a list. See `seeds_flat1`, `seeds_flat2`,
#'   `seeds_flat5`, `seeds_flat9`, `seeds_flat10`, and `seeds_flat11` in
#'   **Examples**.
#' @return
#'   When `flatten` is set to FALSE, a hierarchical list with
#'   `length(level_names)` levels containing a collection of seeds.
#'
#'   When `flatten` is set to TRUE, a hierarchical list with
#'   `length(level_names)` - 1 level(s) containing a collection of seeds (where
#'   "0 levels" corresponds to a named integer vector - see **Details**).
#' @examples
#' seeds_list1 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim"),
#'   n_per_level = c(10)
#' )
#'
#' seeds_list2 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim", "dataset"),
#'   n_per_level = c(10, 15)
#' )
#'
#' seeds_list3 <- seed_maker(
#'   seed = 1234,
#'   level_names = list("sim", c("component_a", "component_b", "component_c")),
#'   n_per_level = c(10, 3)
#' )
#'
#' try(seeds_list4 <- seed_maker(
#'   seed = 1234,
#'   level_names = list("sim", c("component_a", "component_b", "component_c")),
#'   n_per_level = c(10, 4)
#' ))
#'
#' seeds_list5 <- seed_maker(
#'   seed = 1234,
#'   level_names = list(
#'     "sim",
#'     c("component_a", "component_b", "component_c"),
#'     "dataset"
#'   ),
#'   n_per_level = c(10, 3, 5)
#' )
#'
#' seeds_list6 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim"),
#'   n_per_level = c(10),
#'   sample_vector = list(1:100)
#' )
#'
#' seeds_list7 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim", "dataset"),
#'   n_per_level = c(10, 15),
#'   sample_vector = list(1:100, 1:1000)
#' )
#'
#' seeds_list8 <- seed_maker(
#'   seed = 1234,
#'   level_names = list(
#'     "sim",
#'     c("component_a", "component_b", "component_c"),
#'     "dataset"
#'   ),
#'   n_per_level = c(10, 3, 5),
#'   sample_vector = list(1:100, 1:1000, 1:10000)
#' )
#'
#' seeds_list9 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim"),
#'   n_per_level = c(1)
#' )
#'
#' try(seeds_list10 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim"),
#'   n_per_level = c(10),
#'   index_level = FALSE
#' ))
#'
#' seeds_list11 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim"),
#'   n_per_level = c(1),
#'   index_level = FALSE
#' )
#'
#' seeds_list12 <- seed_maker(
#'   seed = 1234,
#'   level_names = list("sim", c("component_a", "component_b", "component_c")),
#'   n_per_level = c(1, 3),
#'   index_level = c(FALSE, TRUE)
#' )
#'
#' seeds_list13 <- seed_maker(
#'   seed = 1234,
#'   level_names = list("sim", c("component_a", "component_b", "component_c")),
#'   n_per_level = c(1, 3),
#'   index_level = c(FALSE, FALSE)
#' )
#'
#' identical(seeds_list12, seeds_list13)
#'
#' seeds_flat1 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim"),
#'   n_per_level = c(10),
#'   flatten = TRUE
#' )
#'
#' is.list(seeds_flat1)
#' is.integer(seeds_flat1)
#'
#' seeds_flat2 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim", "dataset"),
#'   n_per_level = c(10, 15),
#'   flatten = TRUE
#' )
#'
#' is.list(seeds_flat2$sim1)
#' is.integer(seeds_flat2$sim1)
#'
#' seeds_flat5 <- seed_maker(
#'   seed = 1234,
#'   level_names = list(
#'     "sim",
#'     c("component_a", "component_b", "component_c"),
#'     "dataset"
#'   ),
#'   n_per_level = c(10, 3, 5),
#'   flatten = TRUE
#' )
#'
#' is.list(seeds_flat5$sim1$component_a)
#' is.integer(seeds_flat5$sim1$component_a)
#'
#' seeds_flat9 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim"),
#'   n_per_level = c(1),
#'   flatten = TRUE
#' )
#'
#' is.list(seeds_flat9)
#' is.integer(seeds_flat9)
#'
#' try(seeds_flat10 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim"),
#'   n_per_level = c(10),
#'   index_level = FALSE,
#'   flatten = TRUE
#' ))
#'
#' seeds_flat11 <- seed_maker(
#'   seed = 1234,
#'   level_names = c("sim"),
#'   n_per_level = c(1),
#'   index_level = FALSE,
#'   flatten = TRUE
#' )
#'
#' is.list(seeds_flat11)
#' is.integer(seeds_flat11)
#' @export
seed_maker <- function(seed, # nolint cyclocomp_linter
                       level_names,
                       n_per_level,
                       sample_vector = rep(
                         list(rlang::expr(1:1000000)),
                         length(level_names)
                       ),
                       index_level = rep(TRUE, length(level_names)),
                       flatten = FALSE) {

  assertTRUE(
    testCharacter(
      level_names,
      min.chars = 1,
      any.missing = FALSE,
      min.len = 1
    ) |
      testList(
        level_names,
        types = "character",
        any.missing = FALSE,
        min.len = 1
      )
  )
  assertIntegerish(n_per_level, lower = 1, any.missing = FALSE, min.len = 1)
  if (is.list(level_names)) {
    for (i in seq_along(level_names)) {
      if (length(level_names[[i]]) == 1) {
        assertCharacter(
          level_names[[i]],
          min.chars = 1,
          any.missing = FALSE,
          len = 1
        )
      } else {
        assertCharacter(
          level_names[[i]],
          min.chars = 1,
          any.missing = FALSE,
          len = n_per_level[[i]]
        )
      }
    }
  }
  assertList(sample_vector, any.missing = FALSE, len = length(level_names))
  assertLogical(index_level, any.missing = FALSE, len = length(level_names))
  assertLogical(flatten, any.missing = FALSE, len = 1)

  for (i in seq_along(level_names)) {

    if (n_per_level[i] > 1 && !index_level[i] && length(level_names[[i]]) == 1) { # nolint

      rlang::abort(
        "\"index_level\" cannot be \"FALSE\" when the corresponding root name \"n_per_level\" > 1.", # nolint
        class = "SeedMaker"
      )

    }

    if (i == 1) {

      if (index_level[i]) {

        if (length(level_names[[i]]) == 1) {

          seed_frame <-
            tibble::tibble(
              !!level_names[[i]] := paste0(level_names[[i]], 1:n_per_level[i]),
              seed = seed_gen(seed, n_per_level[i], eval(sample_vector[[i]]))
            )

        } else {

          seed_frame <-
            tibble::tibble(
              !!paste0("level_name", i) := level_names[[i]],
              seed = seed_gen(seed, n_per_level[i], eval(sample_vector[[i]]))
            )

        }

      } else {

        if (length(level_names[[i]]) == 1) {

          seed_frame <-
            tibble::tibble(
              !!level_names[[i]] := level_names[[i]],
              seed = seed_gen(seed, n_per_level[i], eval(sample_vector[[i]]))
            )

        } else {

          seed_frame <-
            tibble::tibble(
              !!paste0("level_name", i) := level_names[[i]],
              seed = seed_gen(seed, n_per_level[i], eval(sample_vector[[i]]))
            )

        }

      }

    } else {

      if (index_level[i]) {

        if (length(level_names[[i]]) == 1) {

          seed_frame <- seed_frame %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              !!level_names[[i]] := list(
                paste0(level_names[[i]], 1:n_per_level[i])
              ),
              seed = list(
                seed_gen(seed, n_per_level[i], eval(sample_vector[[i]]))
              )
            ) %>%
            dplyr::ungroup() %>%
            tidyr::unnest(c(!!level_names[[i]], seed)) %>%
            dplyr::relocate(seed, .after = dplyr::everything())

        } else {

          seed_frame <- seed_frame %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              !!paste0("level_name", i) := list(level_names[[i]]),
              seed = list(
                seed_gen(seed, n_per_level[i], eval(sample_vector[[i]]))
              )
            ) %>%
            dplyr::ungroup() %>%
            tidyr::unnest(c(!!paste0("level_name", i), seed)) %>%
            dplyr::relocate(seed, .after = dplyr::everything())

        }

      } else {

        if (length(level_names[[i]]) == 1) {

          seed_frame <- seed_frame %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              !!level_names[[i]] := list(level_names[[i]]),
              seed = list(
                seed_gen(seed, n_per_level[i], eval(sample_vector[[i]]))
              )
            ) %>%
            dplyr::ungroup() %>%
            tidyr::unnest(c(!!level_names[[i]], seed)) %>%
            dplyr::relocate(seed, .after = dplyr::everything())

        } else {

          seed_frame <- seed_frame %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              !!paste0("level_name", i) := list(level_names[[i]]),
              seed = list(
                seed_gen(seed, n_per_level[i], eval(sample_vector[[i]]))
              )
            ) %>%
            dplyr::ungroup() %>%
            tidyr::unnest(c(!!paste0("level_name", i), seed)) %>%
            dplyr::relocate(seed, .after = dplyr::everything())

        }

      }

    }

  }

  level_names_alt <- names(seed_frame)[!names(seed_frame) %in% c("seed")]

  for (j in seq_along(level_names)) {

    if (j == 1 && length(level_names) == 1) {

      if (flatten) {

        seed_list <- seed_frame %>%
          tibble::deframe()

      } else {

        seed_list <- split(seed_frame, ~ eval(parse(text = level_names))) %>%
          lapply(., function(x) x$seed)

        if (length(level_names[[1]]) == 1) {

          seed_list <-
            seed_list[order(as.numeric(substring(
              names(seed_list), nchar(level_names_alt) + 1
            )))]

        } else {

          ord <- tibble::tibble(name = names(seed_list)) %>%
            dplyr::left_join(
              tibble::tibble(name = level_names[[j]]) %>%
                dplyr::mutate(order = dplyr::row_number()),
              by = "name",
              na_matches = "never"
            ) %>%
            dplyr::mutate(ord = dplyr::row_number()) %>%
            dplyr::arrange(.data$order) %>%
            dplyr::pull("ord")

          seed_list <- seed_list[ord]

        }
      }

    } else if (j == 1) {

      if (length(rev(level_names)[[j]]) == 1) {

        seed_list <- seed_frame %>%
          dplyr::group_by(
            dplyr::across(dplyr::all_of(rev(rev(level_names_alt)[-seq(1, j)])))
          ) %>%
          tidyr::nest(a = c(rev(level_names_alt)[j], seed)) %>%
          dplyr::ungroup() %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            a = list(split(.data$a, ~ !!sym(rev(level_names_alt)[j])))
          ) %>%
          dplyr::mutate(a = list(lapply(.data$a, function(x) x$seed))) %>%
          dplyr::mutate(a = list(rlang::exec(
            .fn = function(x) {
              x[order(as.numeric(substring(
                names(x), nchar(rev(level_names_alt)[j]) + 1
              )))]
            },
            x = .data$a
          ))) %>%
          dplyr::ungroup()

      } else {

        seed_list <- seed_frame %>%
          dplyr::group_by(
            dplyr::across(dplyr::all_of(rev(rev(level_names_alt)[-seq(1, j)])))
          ) %>%
          tidyr::nest(a = c(rev(level_names_alt)[j], seed)) %>%
          dplyr::ungroup() %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            a = list(split(.data$a, ~ !!sym(rev(level_names_alt)[j])))
          ) %>%
          dplyr::mutate(a = list(lapply(.data$a, function(x) x$seed))) %>%
          dplyr::mutate(a = list(rlang::exec(
            .fn = function(x) {
              ord <- tibble::tibble(name = names(x)) %>%
                dplyr::left_join(
                  tibble::tibble(name = rev(level_names)[[j]]) %>%
                    dplyr::mutate(order = dplyr::row_number()),
                  by = "name",
                  na_matches = "never"
                ) %>%
                dplyr::mutate(ord = dplyr::row_number()) %>%
                dplyr::arrange(.data$order) %>%
                dplyr::pull("ord")
              x[ord]
            },
            x = .data$a
          ))) %>%
          dplyr::ungroup()

      }

      if (flatten) {

        seed_list <- seed_list %>%
          dplyr::rowwise() %>%
          dplyr::mutate(a = list(rlang::exec(
            .fn = function(x) {
              dplyr::bind_rows(x) %>%
                tidyr::pivot_longer(
                  cols = dplyr::everything(),
                  names_to = "names",
                  values_to = "value"
                ) %>%
                tibble::deframe()
            },
            x = .data$a
          ))) %>%
          dplyr::ungroup()

      }

    } else if (j == length(level_names)) {

      seed_list <-
        split(seed_list, ~ eval(parse(text = rev(level_names_alt)[j]))) %>%
        lapply(., function(x) x$a[[1]])

      if (length(rev(level_names)[[j]]) == 1) {

        seed_list <-
          seed_list[order(as.numeric(substring(
            names(seed_list), nchar(rev(level_names_alt)[j]) + 1
          )))]

      } else {

        ord <- tibble::tibble(name = names(seed_list)) %>%
          dplyr::left_join(
            tibble::tibble(name = rev(level_names)[[j]]) %>%
              dplyr::mutate(order = dplyr::row_number()),
            by = "name",
            na_matches = "never"
          ) %>%
          dplyr::mutate(ord = dplyr::row_number()) %>%
          dplyr::arrange(.data$order) %>%
          dplyr::pull("ord")

        seed_list <- seed_list[ord]

      }

    } else {

      if (length(rev(level_names)[[j]]) == 1) {

        seed_list <- seed_list %>%
          dplyr::group_by(
            dplyr::across(dplyr::all_of(rev(rev(level_names_alt)[-seq(1, j)])))
          ) %>%
          tidyr::nest(a = c(rev(level_names_alt)[j], "a")) %>%
          dplyr::ungroup() %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            a = list(split(.data$a, ~ !!sym(rev(level_names_alt)[j])))
          ) %>%
          dplyr::mutate(a = list(lapply(.data$a, function(x) x$a[[1]]))) %>%
          dplyr::mutate(a = list(rlang::exec(
            .fn = function(x) {
              x[order(as.numeric(substring(
                names(x), nchar(rev(level_names_alt)[j]) + 1
              )))]
            },
            x = .data$a
          ))) %>%
          dplyr::ungroup()

      } else {

        seed_list <- seed_list %>%
          dplyr::group_by(
            dplyr::across(dplyr::all_of(rev(rev(level_names_alt)[-seq(1, j)])))
          ) %>%
          tidyr::nest(a = c(rev(level_names_alt)[j], "a")) %>%
          dplyr::ungroup() %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            a = list(split(.data$a, ~ !!sym(rev(level_names_alt)[j])))
          ) %>%
          dplyr::mutate(a = list(lapply(.data$a, function(x) x$a[[1]]))) %>%
          dplyr::mutate(a = list(rlang::exec(
            .fn = function(x) {
              ord <- tibble::tibble(name = names(x)) %>%
                dplyr::left_join(
                  tibble::tibble(name = rev(level_names)[[j]]) %>%
                    dplyr::mutate(order = dplyr::row_number()),
                  by = "name",
                  na_matches = "never"
                ) %>%
                dplyr::mutate(ord = dplyr::row_number()) %>%
                dplyr::arrange(.data$order) %>%
                dplyr::pull("ord")
              x[ord]
            },
            x = .data$a
          ))) %>%
          dplyr::ungroup()

      }

    }

  }

  return(seed_list)
}
